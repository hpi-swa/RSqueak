from spyvm import model
from spyvm.error import PrimitiveFailedError
from spyvm.shadow import AbstractCachingShadow
from spyvm.plugins.plugin import Plugin

from rpython.rlib import rarithmetic, jit


BitBltPlugin = Plugin()

@BitBltPlugin.expose_primitive(unwrap_spec=[object], clean_stack=False)
def primitiveCopyBits(interp, s_frame, w_rcvr):
    from spyvm.interpreter import Return
    if not isinstance(w_rcvr, model.W_PointersObject) or w_rcvr.size() < 15:
        raise PrimitiveFailedError

    # only allow combinationRules 0-41
    combinationRule = interp.space.unwrap_positive_32bit_int(w_rcvr.fetch(interp.space, 3))
    if combinationRule > 41:
        raise PrimitiveFailedError

    space = interp.space
    s_bitblt = w_rcvr.as_special_get_shadow(space, BitBltShadow)
    s_bitblt.copyBits()

    w_dest_form = w_rcvr.fetch(space, 0)
    if (combinationRule == 22 or combinationRule == 32):
        s_frame.pop() # pops the next value under BitBlt
        s_frame.push(interp.space.wrap_int(s_bitblt.bitCount))
    elif w_dest_form.is_same_object(space.objtable['w_display']):
        w_bitmap = w_dest_form.fetch(space, 0)
        assert isinstance(w_bitmap, model.W_DisplayBitmap)
        w_bitmap.flush_to_screen()
    return w_rcvr

    def as_bitblt_get_shadow(self, space):
        return


class BitBltShadow(AbstractCachingShadow):
    WordSize = 32
    MaskTable = [rarithmetic.r_uint(0)]
    for i in xrange(WordSize):
        MaskTable.append(rarithmetic.r_uint((2 ** (i + 1)) - 1))
    AllOnes = rarithmetic.r_uint(0xFFFFFFFF)

    def sync_cache(self):
        self.loadBitBlt()

    def intOrIfNil(self, w_int, i):
        if w_int is self.space.w_nil:
            return i
        else:
            return self.space.unwrap_int(w_int)

    def loadForm(self, w_form):
        try:
            if not isinstance(w_form, model.W_PointersObject):
                raise PrimitiveFailedError()
            s_form = w_form.as_special_get_shadow(self.space, FormShadow)
            if not isinstance(s_form, FormShadow):
                raise PrimitiveFailedError()
            return s_form
        except PrimitiveFailedError, e:
            w_self = self.w_self()
            assert isinstance(w_self, model.W_PointersObject)
            w_self._shadow = None
            raise e

    def loadHalftone(self, w_halftone_form):
        if w_halftone_form is self.space.w_nil:
            return None
        elif isinstance(w_halftone_form, model.W_WordsObject):
            # Already a bitmap
            return w_halftone_form.words
        else:
            assert isinstance(w_halftone_form, model.W_PointersObject)
            w_bits = w_halftone_form.as_special_get_shadow(self.space, FormShadow).w_bits
            assert isinstance(w_bits, model.W_WordsObject)
            return w_bits.words

    def loadColorMap(self, w_color_map):
        if isinstance(w_color_map, model.W_WordsObject):
            self.w_cmLookupTable = w_color_map
            self.cmMask = self.w_cmLookupTable.size() - 1
        else:
            self.w_cmLookupTable = None

    def loadBitBlt(self):
        self.success = True
        self.w_destForm = self.fetch(0)
        self.dest = self.loadForm(self.w_destForm)
        self.w_sourceForm = self.fetch(1)
        if self.w_sourceForm is not self.space.w_nil:
            self.source = self.loadForm(self.w_sourceForm)
        else:
            self.source = None
        self.halftone = self.loadHalftone(self.fetch(2))
        self.combinationRule = self.space.unwrap_int(self.fetch(3))
        self.destX = self.intOrIfNil(self.fetch(4), 0)
        self.destY = self.intOrIfNil(self.fetch(5), 0)
        self.width = self.intOrIfNil(self.fetch(6), self.dest.width)
        self.height = self.intOrIfNil(self.fetch(7), self.dest.height)
        self.clipX = self.intOrIfNil(self.fetch(10), 0)
        self.clipY = self.intOrIfNil(self.fetch(11), 0)
        self.clipW = self.intOrIfNil(self.fetch(12), self.width)
        self.clipH = self.intOrIfNil(self.fetch(13), self.height)
        if not self.source:
            self.sourceX = 0
            self.sourceY = 0
        else:
            self.loadColorMap(self.fetch(14))
            self.sourceX = self.intOrIfNil(self.fetch(8), 0)
            self.sourceY = self.intOrIfNil(self.fetch(9), 0)

    def copyBits(self):
        self.bitCount = 0
        self.clipRange()
        if (self.bbW <= 0 or self.bbH <= 0):
            return
        self.destMaskAndPointerInit()
        if not self.source:
            self.copyLoopNoSource()
        else:
            self.checkSourceOverlap()
            if self.source.depth != self.dest.depth:
                self.copyLoopPixMap()
            else:
                self.sourceSkewAndPointerInit()
                self.copyLoop()

    def checkSourceOverlap(self):
        if (self.w_sourceForm is self.w_destForm and self.dy >= self.sy):
            if (self.dy > self.sy):
                self.vDir = -1
                self.sy = (self.sy + self.bbH) - 1
                self.dy = (self.dy + self.bbH) - 1
            else:
                if (self.dy == self.sy and self.dx > self.sx):
                    self.hDir = -1
                    self.sx = (self.sx + self.bbW) - 1 # start at right
                    self.dx = (self.dx + self.bbW) - 1
                    if (self.nWords > 1):
                        t = self.mask1 # and fix up masks
                        self.mask1 = self.mask2
                        self.mask2 = t
            self.destIndex = (self.dy * self.dest.pitch) + (self.dx / self.dest.pixPerWord | 0) # recompute since dx, dy change
            self.destDelta = (self.dest.pitch * self.vDir) - (self.nWords * self.hDir)

    def sourceSkewAndPointerInit(self):
        pixPerM1 = self.dest.pixPerWord - 1 # Pix per word is power of two, so self makes a mask
        sxLowBits = self.sx & pixPerM1
        dxLowBits = self.dx & pixPerM1
        # check if need to preload buffer
        # (i.e., two words of source needed for first word of destination)
        dWid = -1
        if (self.hDir > 0):
            if self.bbW < (self.dest.pixPerWord - dxLowBits):
                dWid = self.bbW
            else:
                dWid = self.dest.pixPerWord - dxLowBits
            self.preload = (sxLowBits + dWid) > pixPerM1
        else:
            if self.bbW < (dxLowBits + 1):
                dWid = self.bbW
            else:
                dWid = dxLowBits + 1
            self.preload = ((sxLowBits - dWid) + 1) < 0

        if self.source.msb:
            self.skew = (sxLowBits - dxLowBits) * self.dest.depth
        else:
            self.skew = (dxLowBits - sxLowBits) * self.dest.depth
        if (self.preload):
            if (self.skew < 0):
                self.skew += 32
            else:
                self.skew -= 32
        # calculate increments from end of one line to start of next
        self.sourceIndex = (self.sy * self.source.pitch) + (self.sx / (32 / self.source.depth) |0)
        self.sourceDelta = (self.source.pitch * self.vDir) - (self.nWords * self.hDir)
        if (self.preload):
            self.sourceDelta -= self.hDir

    def clipRange(self):
        # intersect with destForm bounds
        if self.clipX < 0:
            self.clipW += self.clipX
            self.clipX = 0
        if self.clipY < 0:
            self.clipH += self.clipY
            self.clipY = 0
        if self.clipX + self.clipW > self.dest.width:
            self.clipW = self.dest.width - self.clipX
        if self.clipY + self.clipH > self.dest.height:
            self.clipH = self.dest.height - self.clipY
        # intersect with clipRect
        leftOffset = max(self.clipX - self.destX, 0)
        self.sx = self.sourceX + leftOffset
        self.dx = self.destX + leftOffset
        self.bbW = self.width - leftOffset
        rightOffset = (self.dx + self.bbW) - (self.clipX + self.clipW)
        if rightOffset > 0:
            self.bbW -= rightOffset
        topOffset = max(self.clipY - self.destY, 0)
        self.sy = self.sourceY + topOffset
        self.dy = self.destY + topOffset
        self.bbH = self.height - topOffset
        bottomOffset = (self.dy + self.bbH) - (self.clipY + self.clipH)
        if bottomOffset > 0:
            self.bbH -= bottomOffset
        # intersect with sourceForm bounds
        if not self.source:
            return
        if self.sx < 0:
            self.dx -= self.sx
            self.bbW += self.sx
            self.sx = 0
        if (self.sx + self.bbW) > self.source.width:
            self.bbW -= (self.sx + self.bbW) - self.source.width
        if self.sy < 0:
            self.dy -= self.sy
            self.bbH += self.sy
            self.sy = 0
        if (self.sy + self.bbH) > self.source.height:
            self.bbH -= (self.sy + self.bbH) - self.source.height

    def rshift(self, val, n):
        # return rarithmetic.r_uint(val >> n if val >= 0 else (val + 0x100000000) >> n)
        return rarithmetic.r_uint(rarithmetic.r_uint(val) >> n & BitBltShadow.AllOnes)

    def destMaskAndPointerInit(self):
        pixPerM1 = self.dest.pixPerWord - 1 # pixPerWord is power-of-two, so this makes a mask
        startBits = self.dest.pixPerWord - (self.dx & pixPerM1) # how many px in 1st word
        endBits = (((self.dx + self.bbW) - 1) & pixPerM1) + 1
        if self.dest.msb:
            self.mask1 = self.rshift(BitBltShadow.AllOnes, (32 - (startBits * self.dest.depth)))
            self.mask2 = BitBltShadow.AllOnes << (32 - (endBits * self.dest.depth))
        else:
            self.mask1 = BitBltShadow.AllOnes << (32 - (startBits * self.dest.depth))
            self.mask2 = self.rshift(BitBltShadow.AllOnes, (32 - (endBits * self.dest.depth)))
        if self.bbW < startBits:
            self.mask1 = self.mask1 & self.mask2
            self.mask2 = 0
            self.nWords = 1
        else:
            self.nWords = (((self.bbW - startBits) + pixPerM1) / self.dest.pixPerWord | 0) + 1
        self.hDir = 1
        self.vDir = 1
        self.destIndex = (self.dy * self.dest.pitch) + (self.dx / self.dest.pixPerWord | 0)
        self.destDelta = (self.dest.pitch * self.vDir) - (self.nWords * self.hDir)

    def copyLoopNoSource(self):
        halftoneWord = BitBltShadow.AllOnes
        for i in range(self.bbH):
            if self.halftone:
                halftoneWord = rarithmetic.r_uint(self.halftone[(self.dy + i) % len(self.halftone)])
            # first word in row is masked
            destMask = self.mask1
            destWord = self.dest.w_bits.getword(self.destIndex)
            mergeWord = self.mergeFn(halftoneWord, destWord)
            destWord = (destMask & mergeWord) | (destWord & (~destMask))
            self.dest.w_bits.setword(self.destIndex, destWord)
            self.destIndex += 1
            destMask = BitBltShadow.AllOnes
            # the central horizontal loop requires no store masking
            if self.combinationRule == 3: # store rule requires no dest merging
                for word in range(2, self.nWords):
                    self.dest.w_bits.setword(self.destIndex, halftoneWord)
                    self.destIndex += 1
            else:
                for word in range(2, self.nWords):
                    destWord = self.dest.w_bits.getword(self.destIndex)
                    mergeWord = self.mergeFn(halftoneWord, destWord)
                    self.dest.w_bits.setword(self.destIndex, mergeWord)
                    self.destIndex += 1
            # last word in row is masked
            if self.nWords > 1:
                destMask = self.mask2
                destWord = self.dest.w_bits.getword(self.destIndex)
                mergeWord = self.mergeFn(halftoneWord, destWord)
                destWord = (destMask & mergeWord) | (destWord & (~destMask))
                self.dest.w_bits.setword(self.destIndex, destWord)
                self.destIndex += 1
            self.destIndex += self.destDelta

    def copyLoopPixMap(self):
        # This version of the inner loop maps source pixels
        # to a destination form with different depth.  Because it is already
        # unweildy, the loop is not unrolled as in the other versions.
        # Preload, skew and skewMask are all overlooked, since pickSourcePixels
        # delivers its destination word already properly aligned.
        # Note that pickSourcePixels could be copied in-line at the top of
        # the horizontal loop, and some of its inits moved out of the loop.
        #
        # The loop has been rewritten to use only one pickSourcePixels call.
        # The idea is that the call itself could be inlined. If we decide not
        # to inline pickSourcePixels we could optimize the loop instead.
        sourcePixMask = BitBltShadow.MaskTable[self.source.depth]
        destPixMask = BitBltShadow.MaskTable[self.dest.depth]
        self.sourceIndex = (self.sy * self.source.pitch) + (self.sx / self.source.pixPerWord | 0)
        scrStartBits = self.source.pixPerWord - (self.sx & (self.source.pixPerWord - 1))
        if self.bbW < scrStartBits:
            nSourceIncs = 0
        else:
            nSourceIncs = ((self.bbW - scrStartBits) / self.source.pixPerWord | 0) + 1
        # Note following two items were already calculated in destmask setup!
        self.sourceDelta = self.source.pitch - nSourceIncs
        startBits = self.dest.pixPerWord - (self.dx & (self.dest.pixPerWord - 1))
        endBits = (((self.dx + self.bbW) - 1) & (self.dest.pixPerWord - 1)) + 1
        if self.bbW < startBits:
            startBits = self.bbW # ?!
        srcShift = (self.sx & (self.source.pixPerWord - 1)) * self.source.depth
        dstShift = (self.dx & (self.dest.pixPerWord - 1)) * self.dest.depth
        srcShiftInc = self.source.depth
        dstShiftInc = self.dest.depth
        dstShiftLeft = 0
        if (self.source.msb):
            srcShift = (32 - self.source.depth) - srcShift
            srcShiftInc = -srcShiftInc

        if (self.dest.msb):
            dstShift = (32 - self.dest.depth) - dstShift
            dstShiftInc = -dstShiftInc
            dstShiftLeft = 32 - self.dest.depth

        for i in range(self.bbH):
            if self.halftone:
                halftoneWord = rarithmetic.r_uint(self.halftone[(self.dy + i) % len(self.halftone)])
            else:
                halftoneWord = BitBltShadow.AllOnes
            self.srcBitShift = srcShift
            self.dstBitShift = dstShift
            self.destMask = self.mask1
            nPix = startBits
            words = self.nWords
            # Here is the horizontal loop...
            for word in range(words + 1):
                skewWord = self.pickSourcePixels(nPix, sourcePixMask, destPixMask, srcShiftInc, dstShiftInc)
                # align next word to leftmost pixel
                self.dstBitShift = dstShiftLeft
                if self.destMask == BitBltShadow.AllOnes: # avoid read-modify-write
                    self.dest.w_bits.setword(
                        self.destIndex,
                        self.mergeFn(skewWord & halftoneWord, self.dest.w_bits.getword(self.destIndex))
                    )
                else: # General version using dest masking
                    destWord = self.dest.w_bits.getword(self.destIndex)
                    mergeWord = self.mergeFn(skewWord & halftoneWord, destWord & self.destMask)
                    destWord = (self.destMask & mergeWord) | (destWord & (~self.destMask))
                    self.dest.w_bits.setword(self.destIndex, destWord)

                self.destIndex += 1
                if (words == 2): # is the next word the last word?
                    self.destMask = self.mask2
                    nPix = endBits
                else: # use fullword mask for inner loop
                    self.destMask = BitBltShadow.AllOnes
                    nPix = self.dest.pixPerWord
            self.sourceIndex += self.sourceDelta
            self.destIndex += self.destDelta

    def pickSourcePixels(self, nPixels, srcMask, dstMask, srcShiftInc, dstShiftInc):
        # Pick nPix pixels starting at srcBitIndex from the source, map by the
        # color map, and justify them according to dstBitIndex in the resulting destWord.
        sourceWord = rarithmetic.r_uint(self.source.w_bits.getword(self.sourceIndex))
        destWord = 0
        srcShift = self.srcBitShift # put into temp for speed
        dstShift = self.dstBitShift
        nPix = nPixels
        # always > 0 so we can use do { } while(--nPix);
        if (self.w_cmLookupTable): # a little optimization for (pretty crucial) blits using indexed lookups only
            for px in range(nPix + 1):
                sourcePix = self.rshift(rarithmetic.r_uint(sourceWord), srcShift) & srcMask
                destPix = self.w_cmLookupTable.getword(rarithmetic.intmask(sourcePix & self.cmMask))
                # adjust dest pix index
                destWord = destWord | ((destPix & dstMask) << dstShift)
                # adjust source pix index
                dstShift += dstShiftInc
                srcShift += srcShiftInc
                if srcShift & rarithmetic.r_uint(0xFFFFFFE0):
                    if (self.source.msb):
                        srcShift += 32
                    else:
                        srcShift -= 32
                    self.sourceIndex += 1
                    sourceWord = self.source.w_bits.getword(self.sourceIndex)
        else:
            raise PrimitiveFailedError()
        self.srcBitShift = srcShift # Store back
        return destWord

    def rotate32bit(self, thisWord, prevWord, skewMask, notSkewMask, unskew):
        if unskew < 0:
            rotated = self.rshift(rarithmetic.r_uint(prevWord & notSkewMask), -unskew)
        else:
            rotated = (prevWord & notSkewMask) << unskew
        if self.skew < 0:
            rotated = rotated | self.rshift(rarithmetic.r_uint(thisWord & skewMask), -self.skew)
        else:
            rotated = rotated | (thisWord & skewMask) << self.skew
        return rotated

    def copyLoop(self):
        # self version of the inner loop assumes we do have a source
        sourceLimit = self.source.w_bits.size()
        hInc = self.hDir
        # init skew (the difference in word alignment of source and dest)
        if (self.skew == -32):
            self.skew = unskew = 0
            skewMask = rarithmetic.r_uint(0)
        else:
            if (self.skew < 0):
                unskew = self.skew + 32
                skewMask = rarithmetic.r_uint(BitBltShadow.AllOnes << -self.skew)
            else:
                if (self.skew == 0):
                    unskew = 0
                    skewMask = BitBltShadow.AllOnes
                else:
                    unskew = self.skew - 32
                    skewMask = self.rshift(BitBltShadow.AllOnes, self.skew)
        notSkewMask = rarithmetic.r_uint(~skewMask)

        # init halftones
        if (self.halftone):
            halftoneWord = rarithmetic.r_uint(self.halftone[0])
            halftoneHeight = len(self.halftone)
        else:
            halftoneWord = BitBltShadow.AllOnes
            halftoneHeight = 0

        # now loop over all lines
        y = self.dy
        for i in range(1, self.bbH + 1):
            if (halftoneHeight > 1):
                halftoneWord = rarithmetic.r_uint(self.halftone[y % halftoneHeight])
                y += self.vDir

            if (self.preload):
                prevWord = rarithmetic.r_uint(self.source.w_bits.getword(self.sourceIndex))
                self.sourceIndex += hInc
            else:
                prevWord = rarithmetic.r_uint(0)

            destMask = self.mask1
            # pick up next word
            thisWord = rarithmetic.r_uint(self.source.w_bits.getword(self.sourceIndex))
            self.sourceIndex += hInc
            skewWord = self.rotate32bit(thisWord, prevWord, skewMask, notSkewMask, unskew)
            prevWord = thisWord
            destWord = self.dest.w_bits.getword(self.destIndex)
            mergeWord = self.mergeFn(skewWord & halftoneWord, destWord)
            destWord = (destMask & mergeWord) | (destWord & (~destMask))
            self.dest.w_bits.setword(self.destIndex, destWord)
            # The central horizontal loop requires no store masking
            self.destIndex += hInc
            destMask = BitBltShadow.AllOnes
            if (self.combinationRule == 3): # Store mode avoids dest merge function
                if ((self.skew == 0) and (halftoneWord == BitBltShadow.AllOnes)):
                    # Non-skewed with no halftone
                    if (self.hDir == -1):
                        for word in range(2, self.nWords):
                            thisWord = self.source.w_bits.getword(self.sourceIndex)
                            self.dest.w_bits.setword(self.destIndex, thisWord)
                            self.sourceIndex += hInc
                            self.destIndex += hInc
                    else:
                        for word in range(2, self.nWords):
                            self.dest.w_bits.setword(self.destIndex, prevWord)
                            prevWord = self.source.w_bits.getword(self.sourceIndex)
                            self.destIndex += hInc
                            self.sourceIndex += hInc
                else:
                    # skewed and/or halftoned
                    for word in range(2, self.nWords):
                        thisWord = self.source.w_bits.getword(self.sourceIndex)
                        self.sourceIndex += hInc
                        skewWord = self.rotate32bit(thisWord, prevWord, skewMask, notSkewMask, unskew)
                        prevWord = thisWord
                        self.dest.w_bits.setword(self.destIndex, skewWord & halftoneWord)
                        self.destIndex += hInc
            else: # Dest merging here...
                for word in range(2, self.nWords):
                    thisWord = self.source.w_bits.getword(self.sourceIndex) # pick up next word
                    self.sourceIndex += hInc
                    skewWord = self.rotate32bit(thisWord, prevWord, skewMask, notSkewMask, unskew)
                    prevWord = thisWord
                    mergeWord = self.mergeFn(skewWord & halftoneWord, self.dest.w_bits.getword(self.destIndex))
                    self.dest.w_bits.setword(self.destIndex, mergeWord)
                    self.destIndex += hInc
            # last word with masking and all
            if (self.nWords > 1):
                destMask = self.mask2
                if (self.sourceIndex >= 0 and self.sourceIndex < sourceLimit):
                    # NOTE: we are currently overrunning source bits in some cases
                    # self test makes up for it.
                    thisWord = self.source.w_bits.getword(self.sourceIndex) # pick up next word
                self.sourceIndex += hInc
                skewWord = self.rotate32bit(thisWord, prevWord, skewMask, notSkewMask, unskew)
                destWord = self.dest.w_bits.getword(self.destIndex)
                mergeWord = self.mergeFn(skewWord & halftoneWord, destWord)
                destWord = (destMask & mergeWord) | (destWord & (~destMask))
                self.dest.w_bits.setword(self.destIndex, destWord)
                self.destIndex += hInc
            self.sourceIndex += self.sourceDelta
            self.destIndex += self.destDelta

    def mergeFn(self, src, dest):
        return rarithmetic.r_uint(self.merge(
            rarithmetic.r_uint(src),
            rarithmetic.r_uint(dest)
        ))

    def merge(self, source_word, dest_word):
        assert isinstance(source_word, rarithmetic.r_uint) and isinstance(dest_word, rarithmetic.r_uint)
        if self.combinationRule == 0:
            return 0
        elif self.combinationRule == 1:
            return source_word & dest_word
        elif self.combinationRule == 2:
            return source_word & ~dest_word
        elif self.combinationRule == 3:
            return source_word
        elif self.combinationRule == 4:
            return ~source_word & dest_word
        elif self.combinationRule == 5:
            return dest_word
        elif self.combinationRule == 6:
            return source_word ^ dest_word
        elif self.combinationRule == 7:
            return source_word | dest_word
        elif self.combinationRule == 8:
            return ~source_word & ~dest_word
        elif self.combinationRule == 9:
            return ~source_word ^ dest_word
        elif self.combinationRule == 10:
            return ~dest_word
        elif self.combinationRule == 11:
            return source_word | ~dest_word
        elif self.combinationRule == 12:
            return ~source_word
        elif self.combinationRule == 13:
            return ~source_word | dest_word
        elif self.combinationRule == 14:
            return ~source_word | ~dest_word
        elif self.combinationRule >= 15 and self.combinationRule <= 17:
            return dest_word
        elif self.combinationRule == 18:
            return source_word + dest_word
        elif self.combinationRule == 19:
            return source_word - dest_word
        elif self.combinationRule >= 20 and self.combinationRule <= 24:
            return source_word
        elif self.combinationRule == 25:
            if source_word == 0:
                return dest_word
            else:
                return self.partitionedANDtonBitsnPartitions(
                    ~source_word,
                    dest_word,
                    self.dest.depth,
                    self.dest.pixPerWord
                )
        elif self.combinationRule == 26:
            return self.partitionedANDtonBitsnPartitions(
                ~source_word,
                dest_word,
                self.dest.depth,
                self.dest.pixPerWord
            )
        elif 26 < self.combinationRule <= 41:
            return dest_word
        else:
            raise PrimitiveFailedError()

    def partitionedANDtonBitsnPartitions(self, word1, word2, nBits, nParts):
        # partition mask starts at the right
        mask = BitBltShadow.MaskTable[nBits]
        result = 0
        for i in range(1, nParts + 1):
            if ((word1 & mask) == mask):
                result = result | (word2 & mask)
            # slide left to next partition
            mask = mask << nBits
        return result

    def as_string(bb):
        return 'aBitBlt (destX: %d, destY: %d, sx: %d, sy: %d, dx: %d, dy: %d, w: %d, h: %d, hDir: %d, vDir: %d, sDelta: %d, dDelta: %d, skew: %d, sI: %d, dI: %d)' % (
            bb.dest_x, bb.dest_y, bb.sx, bb.sy, bb.dx, bb.dy, bb.w, bb.h, bb.h_dir, bb.v_dir, bb.source_delta, bb.dest_delta, bb.skew, bb.source_index, bb.dest_index)
            # "dest_raster", "source_raster",
            # "halftone_bits", "mask1", "mask2", "skew_mask",
            # "n_words", "preload"


class FormShadow(AbstractCachingShadow):
    _attrs_ = ["w_bits", "width", "height", "depth", "offsetX",
               "offsetY", "msb", "pixPerWord", "pitch"]

    def sync_cache(self):
        if self.size() < 5:
            w_self = self.w_self()
            assert isinstance(w_self, model.W_PointersObject)
            w_self._shadow = None
            raise PrimitiveFailedError
        self.w_bits = self.fetch(0)
        if not (isinstance(self.w_bits, model.W_WordsObject) or isinstance(self.w_bits, model.W_DisplayBitmap)):
            w_self = self.w_self()
            assert isinstance(w_self, model.W_PointersObject)
            w_self._shadow = None
            raise PrimitiveFailedError
        self.width = self.space.unwrap_int(self.fetch(1))
        self.height = self.space.unwrap_int(self.fetch(2))
        self.depth = self.space.unwrap_int(self.fetch(3))
        if self.width < 0 or self.height < 0:
            raise PrimitiveFailedError()
        self.msb = self.depth > 0
        if self.depth < 0:
            self.depth = -self.depth
        if self.depth == 0:
            raise PrimitiveFailedError()
        w_offset = self.fetch(4)
        assert isinstance(w_offset, model.W_PointersObject)
        if not w_offset is self.space.w_nil:
            self.offsetX = self.space.unwrap_int(w_offset._fetch(0))
            self.offsetY = self.space.unwrap_int(w_offset._fetch(1))
        self.pixPerWord = 32 / self.depth
        self.pitch = (self.width + (self.pixPerWord - 1)) / self.pixPerWord | 0
        if self.w_bits.size() != (self.pitch * self.height):
            raise PrimitiveFailedError()
