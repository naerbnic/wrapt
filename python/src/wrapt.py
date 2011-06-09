import threading
import struct
import itertools

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
  
  def getRootHandle(self):
    return self.__wrapt_factory.createHandle(self.__object_store, 0)

  def createHandle(self):
    return self.__wrapt_factory.createHandle(self.__object_store, self.__object_store.allocateIndex())

  def write(self, filename):
    raise NotImplementedError()


class WraptMapBuilder:
  def __init__(self, dest_cell):
    self.__dest_cell = dest_cell
    self.__entries = []
    self.__has_built = False

  def put(self, key, handle):
    self.__entries.append((key, handle))

  def build(self):
    if self.__has_built:
      raise ValueError("Tried to build a map twice on the same builder")
    self.__dest_cell.setObject('map', self.__entries)
    self.__has_built = True


class WraptMap(dict):
  def __init__(self, entries):
    for key, value in entries:
      super(WraptMap, self).__setitem__(key, value)

  def __setitem__(self, key, value):
    raise ValueError("Cannot set a wrapt map directly")


class WraptArrayMapFactory:
  def __init__(self, object_store):
    self.__object_store = object_store

  def createMapBuilder(self, object_cell):
    return WraptMapBuilder(object_cell)

  def createMap(self, rawMap):
    return WraptMap(rawMap)


class WraptHandleFactory:
  def createHandle(self, object_store, index):
    return WraptHandle(WraptObjectCell(object_store, index), WraptArrayMapFactory(object_store))


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
    type_id, value = self.__object_cell.getObject()
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
    type_id, value = self.__object_cell.getObject()
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
    return self.__handle_factory.createMapBuilder(self.__object_cell)

  def createArray(self):
    return self.__handle_factory.createArrayBuilder(self.__object_cell)

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

  def setObject(self, index, type_id, data):
    if index >= self.__max_index:
      raise WraptIndexOutOfBoundsException
    self.__overrides[index] = (type_id, data)

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
  def __init__(self, low_level_file):
    self.__low_level_file = low_level_file

  def __getObjectValue(self, type_id, data):
    if type_id == 'int':
      return self.__readInt(data),
    elif type_id == 'float':
      return self.__readFloat(data)
    elif type_id == 'string':
      return self.__readString(data)
    elif type_id == 'boolean':
      return self.__readBoolean(data)
    elif type_id == 'map':
      return self.__readMap(data)
    elif type_id == 'array':
      return self.__readArray(data)
    elif type_id == 'null':
      return self.__readNull(data)
    elif type_id == 'blob':
      return self.__readBlob(data)
    else:
      raise WraptFileFormatException()

  def getObject(self, index):
    type_id, data = self.__low_level_file.getObject(index)
    return (type_id, self.__getObjectValue(type_id, data))

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
    else:
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

  INT_TYPE = 0b000
  FLOAT_TYPE = 0b001
  STRING_TYPE = 0b010
  BOOLEAN_TYPE = 0b011
  MAP_TYPE = 0b100
  ARRAY_TYPE = 0b101
  NULL_TYPE = 0b110
  BLOB_TYPE = 0b111

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

  def __toTypeId(typecode):
    if typecode == INT_TYPE:
      return 'int'
    elif typecode == FLOAT_TYPE:
      return 'float'
    elif typecode == STRING_TYPE:
      return 'string'
    elif typecode == BOOLEAN_TYPE:
      return 'boolean'
    elif typecode == MAP_TYPE:
      return 'map'
    elif typecode == ARRAY_TYPE:
      return 'array'
    elif typecode == NULL_TYPE:
      return 'null'
    elif typecode == BLOB_TYPE:
      return 'blob'
    else:
      raise WraptFileFormatException()

  def getObject(self, index):
    index_offset, typecode = self.__getIndexInfo(index);
    type_id = self._toTypeId(typecode)

    try:
      next_offset, _ = self.__getIndexInfo(index + 1)
    except WraptIndexOutOfBoundsException:
      next_offset = self.__file_size - self.__data_offset
    data_size = next_offset - index_offset

    object_data = self.__bin_file.readBytes(self.__data_offset + index_offset)

    return type_id, object_data

  def getObjectCount(self):
    return self.__max_index

# Writes data contained in a store to a writer.
class WraptOutputProcessor:
  def __init__(self, object_store):
    self.__object_store = object_store    

  def write(self, writer):
    index_map = IndexAllocator(self.__object_store).allocate()

    writer_allocations = [(output_index, store_index) 
        for store_index, output_index in index_map.items()]

    writer_allocations.sort()

    for output_index, store_index in writer_allocations:
      type_id, value = self.__object_store.getObject(store_index)
      self.__write_object(type_id, value, output_index, index_map, writer)

  def __write_object(self, type_id, value, base_index, index_map, writer):
    if type_id == 'int':
      writer.appendObject('int', value)
    elif type_id == 'float':
      writer.appendObject('float', value)
    elif type_id ==  'map':
      for new_type_id, value in value.getBaseObjects(base_index, index_map):
        writer.appendObject(new_type_id, value)
    elif type_id == 'null':
      writer.appendObject('null', value)
    else:
      raise ValueError("Don't support type '{0}'".format(type_id))

class IndexAllocator:
  def __init__(self, object_store):
    self.__object_store = object_store
    self.__allocations = {}
    self.__next_index = 0

  def allocate(self):
    self.__allocate_index(0)
    return self.__allocations

  def __allocate_index(self, index):
    if index in self.__allocations:
      return self.__allocations[index]

    self.__allocations[index] = self.__next_index
    type_id, value = self.__object_store.getObject(index)
    self.__next_index += self.__get_index_size(type_id, value)
    self.__allocate_contents(type_id, value)

  def __get_index_size(self, type_id, value):
    if type_id == 'map':
      return value.getIndexCount()
    elif type_id == 'float':
      return 1
    elif type_id == 'int':
      return 1
    elif type_id == 'null':
      return 1
    else:
      raise ValueError("Does not support type '{0}'".format(type_id))

  def __allocate_contents(self, type_id, value):
    if type_id == 'float':
      pass
    elif type_id == 'int':
      pass
    elif type_id == 'map':
      for next_index in value.getContentIndexes():
        self.__allocate_index(next_index)
      return
    elif type_id == 'null':
      pass
    else:
      raise ValueError("Does not support type '{0}'".format(type_id))


class WraptRefMap:
  def __init__(self, tag, hash_data_index, string_ref_map):
    self.__tag = tag
    self.__hash_data_index = hash_data_index
    self.__string_ref_map = string_ref_map

  def getIndexCount(self):
    return 2 + len(self.__string_ref_map)

  def getContentIndexes(self):
    return itertools.chain(self.__string_ref_map.values(), [self.__hash_data_index])

  def getBaseObjects(self, base_index, index_map):
    field_list = list(self.__string_ref_map.items())

    base_field_list = [(base_index + 2 + i, index_map[value_index])
      for i, (key, value_index) in enumerate(field_list)]

    field_objects = [('string', key) for (key, _) in field_list]
      
    return [
      ('map', WraptBaseMap(
        base_index + 1, #tag
        index_map[self.__hash_data_index],
        base_field_list)),
      ('string', self.__tag)] + field_objects


class WraptBaseMap:
  def __init__(self, tag_index, hash_data_index, field_pairs):
    self.__tag_index = tag_index
    self.__hash_data_index = hash_data_index
    self.__field_pairs = field_pairs

  def getTagIndex(self):
    return self.__tag_index

  def getHashDataIndex(self):
    return self.__hash_data_index

  def getMappings(self):
    return self.__field_pairs
