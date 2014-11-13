from rpython.rlib import streamio

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
    
    def peek(self):
        if self.pos >= len(self.data):
            raise IndexError
        data_peek = self.data[self.pos:self.pos + self.word_size]
        if self.use_long_read:
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
