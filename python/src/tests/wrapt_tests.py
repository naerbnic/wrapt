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

  def test_setRootToMap(self):
    newHandle = self.__file.createHandle()
    newHandle.createInt(12)

    root = self.__file.getRootHandle()
    builder = root.createMap()
    builder.put('hello', newHandle)
    builder.build()

    wraptMap = root.asMap()

    self.assertIsNot(wraptMap, None)
    self.assertEqual(wraptMap.getEntryCount(), 1)
    
    helloHandle = wraptMap.get('hello')
    self.assertIsNot(helloHandle, None)
    self.assertEqual(helloHandle.asInt(), 12)

    foundHello = False
    for key, handle in wraptMap.items():
      if key == 'hello':
        self.assertEqual(helloHandle, handle)
        foundHello = True
      else:
        self.fail(msg="Unexpected item found in map")

    self.assertTrue(foundHello, msg="Did not find 'hello' in map")

    
