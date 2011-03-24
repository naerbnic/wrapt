import threading
import struct

class WraptFileFormatException:
  pass

class WraptIndexOutOfBoundsException:
  pass

class WraptFileIOError:
  pass


class WraptFile(object):
  def __init__(self, low_level_file, handle_factory):
    self.__low_level_file = low_level_file
    self.__overrides = {} #Maps indexes to new objects
    self.__fresh_object_index = self.__low_level_file.getObjectCount()
    self.__handle_factory = handle_factory


class ByteArrayBinaryFile:
  def __init__(self, __byte_array):
    self.__byte_array = __byte_array

  def readBytes(self, offset, length):
    return self.__byte_array[offset, offset + length]

  def getLength(self):
    return len(self.__byte_array)


class FileObjectBinaryFile:
  def __init__(self, fileobj):
    self.__file = fileobj
    self.__file_lock = threading.RLock()

  def readBytes(self, offset, length):
    with self.__file_lock:
      self.__file.seek(offset)
      result = self.__file.read(length)
      if len(result) != length:
        raise WraptFileIoError()
      return result

  def getLength(self):
    with self.__file_lock:
      self.__file.seek(0, os.SEEK_END)
      return self.__file.tell()
    
class WraptLowLevelFile:
  __header_format = struct.Struct('!8cQ')
  HEADER_SIZE = 16
  INDEX_ENTRY_SIZE = 8

  def __init__(self, binfile):
    self.__bin_file = binfile; 
    self.__data_offset = None;
    self.__max_index = None;

  def initialize(self):
    header_data = self.__bin_file.readBytes(0, HEADER_SIZE)
    self.__file_size = self.__bin_file.getLength()
    magic_number, self.__data_offset = self.__header_format.unpack(header_data)
    if (magic_number != 'WraptDat')
      raise WraptFileFormatException() 
    self.__max_index = (self.__data_offset - HEADER_SIZE) / INDEX_ENTRY_SIZE

  def __getIndexEntry(self, index):
    if index >= self.__max_index:
      raise WraptIndexOutOfBoundsException()
    offset = HEADER_SIZE + index * INDEX_ENTRY_SIZE
    return struct.unpack('!Q', self.__bin_file.readBytes(offset, INDEX_ENTRY_SIZE))

  def __getIndexInfo(self, index):
    data = self.__getIndexEntry(index):
    offset = data & 0xfffffff8
    typecode = data & 0x7
    return offset, typecode

  def readObject(self, index):
    index_offset, type = self.__getIndexInfo(index);
    try:
      next_offset, _ = self.__getIndexInfo(index + 1):
    except WraptIndexOutOfBoundsException:
      next_offset = self.__file_size - self.__data_offset
    data_size = next_offset - index_offset

    object_data = self.__bin_file.readBytes(self.__data_offset + index_offset)

    return object_data, type

  def getObjectCount(self):
    return self.__max_index
