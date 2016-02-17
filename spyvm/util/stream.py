from rpython.rlib import streamio, objectmodel
from rpython.rlib.rstruct.runpack import runpack
from spyvm.util import system

if not objectmodel.we_are_translated():
    def chrs2int(b):
        assert len(b) == 4
        first = ord(b[0]) # big endian
        if first & 0x80 != 0:
            first = first - 0x100
        return (first << 24 | ord(b[1]) << 16 | ord(b[2]) << 8 | ord(b[3]))

    def swapped_chrs2int(b):
        assert len(b) == 4
        first = ord(b[3]) # little endian
        if first & 0x80 != 0:
            first = first - 0x100
        return (first << 24 | ord(b[2]) << 16 | ord(b[1]) << 8 | ord(b[0]))

    def chrs2long(b):
        assert len(b) == 8
        first = ord(b[0]) # big endian
        if first & 0x80 != 0:
            first = first - 0x100
        return (      first << 56 | ord(b[1]) << 48 | ord(b[2]) << 40 | ord(b[3]) << 32
                      | ord(b[4]) << 24 | ord(b[5]) << 16 | ord(b[6]) <<  8 | ord(b[7])      )

    def swapped_chrs2long(b):
        assert len(b) == 8
        first = ord(b[7]) # little endian
        if first & 0x80 != 0:
            first = first - 0x100
        return (      first << 56 | ord(b[6]) << 48 | ord(b[5]) << 40 | ord(b[4]) << 32
                      | ord(b[3]) << 24 | ord(b[2]) << 16 | ord(b[1]) <<  8 | ord(b[0])      )

    def bytes2dword(b): # same as chrs2long, but unsigned
        assert len(b) == 8
        return (ord(b[0]) << 56 | ord(b[1]) << 48 | ord(b[2]) << 40 | ord(b[3]) << 32
                | ord(b[4]) << 24 | ord(b[5]) << 16 | ord(b[6]) <<  8 | ord(b[7]))

    def swapped_bytes2dword(b):
        assert len(b) == 8
        return (ord(b[7]) << 56 | ord(b[6]) << 48 | ord(b[5]) << 40 | ord(b[4]) << 32
                | ord(b[3]) << 24 | ord(b[2]) << 16 | ord(b[1]) <<  8 | ord(b[0])      )

    def bytes2qword(b):
        assert len(b) == 16
        return (ord(b[0]) << 120 | ord(b[1]) << 112 | ord(b[2]) << 104 | ord(b[3]) << 96
                | ord(b[4]) << 88 | ord(b[5]) << 80 | ord(b[6]) << 72 | ord(b[7]) << 64
                | ord(b[8]) << 56 | ord(b[9]) << 48 | ord(b[10]) << 40 | ord(b[11]) << 32
                | ord(b[12]) << 24 | ord(b[13]) << 16 | ord(b[14]) <<  8 | ord(b[15]))

    def swapped_bytes2qword(b):
        assert len(b) == 16
        return (ord(b[15]) << 120 | ord(b[14]) << 112 | ord(b[13]) << 104 | ord(b[12]) << 96
                | ord(b[11]) << 88 | ord(b[10]) << 80 | ord(b[9]) << 72 | ord(b[8]) << 64
                | ord(b[7]) << 56 | ord(b[6]) << 48 | ord(b[5]) << 40 | ord(b[4]) << 32
                | ord(b[3]) << 24 | ord(b[2]) << 16 | ord(b[1]) <<  8 | ord(b[0]))
else:
    def chrs2int(b):
        assert len(b) == 4
        return runpack('>i', b)

    def swapped_chrs2int(b):
        assert len(b) == 4
        return runpack('<i', b)

    def chrs2long(b):
        assert len(b) == 8
        return runpack('>q', b)

    def swapped_chrs2long(b):
        assert len(b) == 8
        return runpack('<q', b)

    def bytes2dword(b):
        return runpack(">I", b)

    def swapped_bytes2dword(b):
        return runpack("<I", b)

    def bytes2qword(b):
        return runpack(">Q", b)

    def swapped_bytes2qword(b):
        return runpack("<Q", b)


class Stream(object):
    """ Simple input stream.
    Data is completely read into memory.
    Constructor can raise OSError. """

    def __init__(self, filename=None, inputfile=None, data=None):
        if filename:
            f = streamio.open_file_as_stream(filename, mode="rb", buffering=0)
            try:
                self.data = f.readall()
            finally:
                f.close()
        elif inputfile:
            try:
                self.data = inputfile.read()
            finally:
                inputfile.close()
        elif data:
            self.data = data
        else:
            raise RuntimeError("need to supply either inputfile or data")

        self.reset()

    def bytes2dword_with_correct_endianness(self, bytes):
        if self.big_endian:
            return bytes2dword(bytes)
        else:
            return swapped_bytes2dword(bytes)

    def bytes2qword_with_correct_endianness(self, bytes):
        if self.big_endian:
            return bytes2qword(bytes)
        else:
            return swapped_bytes2qword(bytes)

    def peek_bytes(self, n):
        return self.data[self.pos:self.pos+n]

    def next_bytes(self, n):
        bytes = self.peek_bytes(n)
        self.pos += n
        self.count += n
        return bytes

    def peek(self):
        if self.pos >= len(self.data):
            raise IndexError
        data_peek = self.data[self.pos:self.pos + self.word_size]
        if self.use_long_read:
            assert system.IS_64BIT, "do not support reading 64 bit slots in 32 bit build"
            if self.big_endian:
                return chrs2long(data_peek)
            else:
                return swapped_chrs2long(data_peek)
        else:
            if self.big_endian:
                return chrs2int(data_peek)
            else:
                return swapped_chrs2int(data_peek)

    def next(self):
        integer = self.peek()
        self.pos += self.word_size
        self.count += self.word_size
        return integer

    def next_short(self):
        short = self.peek() >> (self.word_size * 8 - 16)
        self.pos += 2
        self.count += 2
        return short

    def next_qword(self):
        bytes = self.next_bytes(8)
        qword = self.bytes2qword_with_correct_endianness(bytes)
        return qword

    def reset(self):
        self.big_endian = True
        self.pos = 0
        self.count = 0
        self.be_32bit()

    def reset_count(self):
        self.count = 0

    def skipbytes(self, jump):
        assert jump > 0
        assert (self.pos + jump) <= len(self.data)
        self.pos += jump
        self.count += jump

    def skipwords(self, jump):
        self.skipbytes(jump * self.word_size)
        assert (self.pos + jump) <= len(self.data)
        self.pos += jump
        self.count += jump

    def length(self):
        return len(self.data)

    def close(self):
        pass # already closed

    def be_64bit(self):
        self.word_size = 8
        self.use_long_read = True

    def be_32bit(self):
        self.word_size = 4
        self.use_long_read = False
