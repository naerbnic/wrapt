import unittest
import wrapt

class SimpleObjectCell:
  def __init__(self):
    self.__type_id = None
    self.__value = None

  def getObject(self):
    return (self.__type_id, self.__value)

  def setObject(self, type_id, value):
    self.__type_id = type_id
    self.__value = value

class WraptHandleGetterTest(unittest.TestCase):
  def setUp(self):
    self.__cell = SimpleObjectCell()
    self.__handle = wrapt.WraptHandle(self.__cell, None)

  def test_getTypedValue(self):
    self.__cell.setObject('int', 5)
    self.assertEqual(self.__handle.asInt(), 5)
    self.__cell.setObject('float', 1.0)
    self.assertIs(self.__handle.asInt(), None)

  def test_getNull(self):
    self.__cell.setObject('null', None)
    self.assertTrue(self.__handle.isNull())
    self.__cell.setObject('float', 1.0)
    self.assertFalse(self.__handle.isNull())

class InMemoryObjectStore:
  def __init__(self):
      self.__store = [None]

  def getObject(self, index):
    return self.__store[index]

  def setObject(self, index, type_id, value):
    self.__store[index] = (type_id, value)

  def allocateIndex(self):
    result_index = len(self.__store)
    self.__store.append(None)
    return result_index

  def _setData(self, *args):
    self.__store = list(args)

class CreateLiveWraptFileTest(unittest.TestCase):
  def _createObjectStore(self):
    return InMemoryObjectStore()

  def setUp(self):
    self.__file = wrapt.WraptFile(self._createObjectStore(), wrapt.WraptHandleFactory())

  def test_setRootToInteger(self):
    handle = self.__file.getRootHandle()
    handle.createInt(5)

    self.assertEqual(handle.asInt(), 5)

  def test_setRootToFloat(self):
    handle = self.__file.getRootHandle()
    handle.createFloat(2.0)

    self.assertEqual(handle.asFloat(), 2.0)
    self.assertIs(handle.asBlob(), None)
    self.assertFalse(handle.isNull())


  def test_setRootToMapHello(self):
    self.assertSetRootToMap('hello')

  def test_setRootToMapGoodbye(self):
    self.assertSetRootToMap('goodbye')

  def assertSetRootToMap(self, keyname):
    new_handle = self.__file.createHandle()
    new_handle.createInt(12)

    root = self.__file.getRootHandle()
    builder = root.createMap()
    builder.put(keyname, new_handle)
    builder.build()

    wrapt_map = root.asMap()

    self.assertIsNot(wrapt_map, None)
    self.assertEqual(len(wrapt_map), 1)
    
    hello_handle = wrapt_map[keyname]
    self.assertIsNot(hello_handle, None)
    self.assertEqual(hello_handle.asInt(), 12)

    self.assertEntriesInMap(wrapt_map,
      (keyname, hello_handle))

  def insertHelloHandle(self, map_builder):
    new_handle = self.__file.createHandle()
    new_handle.createInt(12)
    map_builder.put('hello', new_handle)

  def insertGoodbyeHandle(self, map_builder):
    new_handle = self.__file.createHandle()
    new_handle.createFloat(2.0)
    map_builder.put('goodbye', new_handle)

  def assertHelloHandle(self, wrapt_map): 
    hello_handle = wrapt_map['hello']
    self.assertIsNot(hello_handle, None)
    self.assertEqual(hello_handle.asInt(), 12)
    return hello_handle

  def assertGoodbyeHandle(self, wrapt_map):
    goodbye_handle = wrapt_map['goodbye']
    self.assertIsNot(goodbye_handle, None)
    self.assertEqual(goodbye_handle.asFloat(), 2.0)
    return goodbye_handle

  def assertEntriesInMap(self, wrapt_map, *args):
    found_array = [False] * len(args)
    for key, value in wrapt_map.items():
      inExpectedValues = False
      for i, (expected_key, expected_value) in enumerate(args):
        if key == expected_key:
          self.assertEqual(value, expected_value)
          found_array[i] = True
          inExpectedValues = True
      self.assertTrue(inExpectedValues,
          msg="Unexpected entry {0} found".format(key))

    for i, found in enumerate(found_array):
      self.assertTrue(found,
          msg="Did not find key {0} in map".format(args[i][0]))

  def test_SetRootToMapTwoElements(self):
    root = self.__file.getRootHandle()
    builder = root.createMap()
    self.insertHelloHandle(builder)
    self.insertGoodbyeHandle(builder)
    builder.build()

    wrapt_map = root.asMap()

    self.assertIsNot(wrapt_map, None)
    self.assertEqual(len(wrapt_map), 2)

    hello_handle = self.assertHelloHandle(wrapt_map)
    goodbye_handle = self.assertGoodbyeHandle(wrapt_map)

    self.assertEntriesInMap(wrapt_map,
      ('hello', hello_handle),
      ('goodbye', goodbye_handle))

  def test_doubleBuildFails(self):
    root = self.__file.getRootHandle()
    builder = root.createMap();
    self.insertHelloHandle(builder);
    builder.build()

    with self.assertRaises(ValueError):
      builder.build()

  def test_mapsAreImmutable(self):
    root = self.__file.getRootHandle()
    builder = root.createMap();
    self.insertHelloHandle(builder);
    builder.build()

    m = root.asMap()

    with self.assertRaises(ValueError):
      m['goodbye'] = 5;

    with self.assertRaises(TypeError):
      m.items()[0] = 5

class InMemoryObjectWriter:
  def __init__(self):
    self.__objects = []

  def appendObject(self, type_id, value):
    self.__objects.append((type_id, value))

  def toList(self):
    return list(self.__objects)

class WraptOutputProcessorTest(unittest.TestCase):
  def setUp(self):
    self.__store = InMemoryObjectStore()
    self.__processor = wrapt.WraptOutputProcessor(self.__store)

  def test_initProcessor(self):
    pass

  def assertSimpleStore(self, type_id, value):
    self.__store._setData((type_id, value))
    writer = InMemoryObjectWriter()
    self.__processor.write(writer)
    objects = writer.toList()

    self.assertEqual(len(objects), 1)
    self.assertEqual(objects[0], (type_id, value))

  def test_storeSimple1(self):
    self.assertSimpleStore('int', 5)

  def test_storeSimple2(self):
    self.assertSimpleStore('int', 6)
