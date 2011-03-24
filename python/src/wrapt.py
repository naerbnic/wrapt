import threading
import struct

class WraptFileFormatException(Exception):
  pass

class WraptIndexOutOfBoundsException(Exception):
  pass

class WraptFileIOError(Exception):
  pass


class WraptFile(object):
  def __init__(self, object_store, wrapt_factory):
    self.__object_store = object_store
    self.__wrapt_factory = wrapt_factory
  
  def readRootHandle(self):
    return self.__wrapt_factory.createHandle(self.__object_store, 0)

  def createHandle(self):
    return self.__wrapt_factory(self.__object_store, self.__object_store.allocateIndex())

  def write(self, filename):
    raise NotImplementedError()

class WraptObjectCell:
  def __init__(self, object_store, index):
    self.__object_store = object_store
    self.__index = index

  def getObject(self):
    return self.__object_store.getObject(self.__index)

  def setObject(self, type_id, value):
    self.__object_store.setObject(self.__index, type_id, value)

class WraptHandle:
  def __init__(self, object_cell, handle_factory):
    self.__object_cell = object_cell
    self.__handle_factory = handle_factory 

  def __getTypedObject(self, expected_type_id):
    value, type_id = self.__getObject()
    if type_id != expected_type_id:
      return None
    else:
      return value
  
  def asInt(self):
    return self.__getTypedObject('int')

  def asFloat(self):
    return self.__getTypedObject('float')

  def asBoolean(self):
    return self.__getTypedObject('boolean')

  def asString(self):
    return self.__getTypedObject('string')

  def asMap(self):
    return self.__handle_factory.createMap(self.__getTypedObject('map')) 

  def asArray(self):
    return self.__handle_factory.createArray(self.__getTypedObject('array'))

  def isNull(self):
    value, type_id = self.__object_cell.getObject()
    return type_id == 'null'

  def asBlob(self):
    return self.__getTypedObject('blob')

  def createInt(self, value):
    self.__object_cell.setObject('int', value)

  def createFloat(self, value):
    self.__object_cell.setObject('float', value)

  def createString(self, value):
    self.__object_cell.setObject('string', value)

  def createBoolean(self, value):
    self.__object_cell.setObject('boolean', value)

  def createMap(self):
    return self.__handle_factory.createMapBuilder(object_builder)

  def createArray(self):
    return self.__handle_factory.createArrayBuilder(array_builder)

  def createNull(self):
    self.__object_cell.setObject('null', None)

  def createBlob(self, value):
    self.__object_cell.setObject('blob', value)


class WraptObjectStore:
  def __init__(self, object_file):
    self.__object_file = object_file
    self.__max_index = object_file.getMaxIndex()
    self.__overrides = {}

  def getObject(self, index):
    if index in self.__overrides:
      return self.__overrides[index]
    else:
      return self.__object_file.getObject(index)

  def setObject(self, index, type_id, data)
    if index >= self.__max_index:
      raise WraptIndexOutOfBoundsException
    self.__overrides[index] = (data, type_id)

  def allocateIndex(self):
    result_index = self.__max_index
    self.__max_index += 1
    return result_index

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


class InvalidTypeException(Exception):
  pass


class WraptCachedObjectFile:
  def __init__(self, delegate):
    self.__cache = {}
    self.__delegate = delegate

  def getObject(self, index):
    if index not in self.__cache:
      self.__cache[index] = self.__delegate.getObject(index)
    return self.__cache[index]

  def getObjectCount(self):
    return self.__delegate.getObjectCount()


class WraptObjectFile:
  INT_TYPE = 0b000
  FLOAT_TYPE = 0b001
  STRING_TYPE = 0b010
  BOOLEAN_TYPE = 0b011
  MAP_TYPE = 0b100
  ARRAY_TYPE = 0b101
  NULL_TYPE = 0b110
  BLOB_TYPE = 0b111

  def __init__(self, low_level_file):
    self.__low_level_file = low_level_file

  def getObject(self, index, expected_typecode):
    data, typecode = self.__low_level_file.readObject(index)
    if typecode == INT_TYPE:
      return (self.__readInt(data), 'int')
    elif typecode == FLOAT_TYPE:
      return (self.__readFloat(data), 'float')
    elif typecode == STRING_TYPE:
      return (self.__readString(data), 'string')
    elif typecode == BOOLEAN_TYPE:
      return (self.__readBoolean(data), 'boolean')
    elif typecode == MAP_TYPE:
      return (self.__readMap(data), 'map')
    elif typecode == ARRAY_TYPE:
      return (self.__readArray(data), 'array')
    elif typecode == NULL_TYPE:
      return (self.__readNull(data), 'null')
    elif typecode == BLOB_TYPE:
      return (self.__readBlob(data), 'blob')
    else:
      raise WraptFileFormatException()

  def getObjectCount(self):
    return self.__low_level_file.getObjectCount()
  
  def __readInt(self, data):
    if len(data) == 8:
      return struct.unpack("!q", data)
    else:
      raise WraptFileFormatException(); # TODO(brianchin): Temporary error. Should calculate long here

  def __readFloat(self, data):
    if len(data) == 8:
      return struct.unpack("!d", data)
    else:
      raise WraptFileFormatException()

  def __readString(self, data):
    return data.decode("utf-8")

  def __readBoolean(self, data):
    if len(data) != 8:
      raise WraptFileFormatException()
    boolean_value = struct.unpack("!Q", data)
    if boolean_value == 0:
      return False
    elif boolean_value == 1:
      return True
    else
      raise WraptFileFormatException()
    
  def __readMap(self, mapData):
    size = len(mapData)
    num_entries = (size - 16) // 16

    tag_index, hash_data_index = struct.unpack("!QQ", mapData[0:16])

    hashData = self.readBlob(hash_data_index)
    tag = self.readString(tag_index)
    return WraptBinaryMap(tag, hashData, mapData[16:])

  def __readArray(self, data):
    return WrapBinaryArray(data)

  def __readNull(self, data):
    if len(data) != 0:
      raise WraptFileFormatException()
    return None

  def __readBlob(self, data):
    return data
    

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
    if magic_number != 'WraptDat':
      raise WraptFileFormatException() 
    self.__max_index = (self.__data_offset - HEADER_SIZE) // INDEX_ENTRY_SIZE

  def __getIndexEntry(self, index):
    if index >= self.__max_index:
      raise WraptIndexOutOfBoundsException()
    offset = HEADER_SIZE + index * INDEX_ENTRY_SIZE
    return struct.unpack('!Q', self.__bin_file.readBytes(offset, INDEX_ENTRY_SIZE))

  def __getIndexInfo(self, index):
    data = self.__getIndexEntry(index)
    offset = data & 0xfffffff8
    typecode = data & 0x7
    return offset, typecode

  def readObject(self, index):
    index_offset, typecode = self.__getIndexInfo(index);
    try:
      next_offset, _ = self.__getIndexInfo(index + 1)
    except WraptIndexOutOfBoundsException:
      next_offset = self.__file_size - self.__data_offset
    data_size = next_offset - index_offset

    object_data = self.__bin_file.readBytes(self.__data_offset + index_offset)

    return object_data, typecode

  def getObjectCount(self):
    return self.__max_index
