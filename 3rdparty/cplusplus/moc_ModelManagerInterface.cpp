/****************************************************************************
** Meta object code from reading C++ file 'ModelManagerInterface.h'
**
** Created: Mon Jul 18 22:51:58 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.4)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "ModelManagerInterface.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'ModelManagerInterface.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.4. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_CPlusPlus__CppModelManagerInterface[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: signature, parameters, type, tag, flags
      41,   37,   36,   36, 0x05,
      89,   83,   36,   36, 0x05,

 // slots: signature, parameters, type, tag, flags
     123,   36,   36,   36, 0x0a,
     177,  165,  151,   36, 0x0a,
     208,   36,   36,   36, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_CPlusPlus__CppModelManagerInterface[] = {
    "CPlusPlus::CppModelManagerInterface\0"
    "\0doc\0documentUpdated(CPlusPlus::Document::Ptr)\0"
    "files\0sourceFilesRefreshed(QStringList)\0"
    "updateModifiedSourceFiles()\0QFuture<void>\0"
    "sourceFiles\0updateSourceFiles(QStringList)\0"
    "GC()\0"
};

const QMetaObject CPlusPlus::CppModelManagerInterface::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_CPlusPlus__CppModelManagerInterface,
      qt_meta_data_CPlusPlus__CppModelManagerInterface, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &CPlusPlus::CppModelManagerInterface::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *CPlusPlus::CppModelManagerInterface::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *CPlusPlus::CppModelManagerInterface::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_CPlusPlus__CppModelManagerInterface))
        return static_cast<void*>(const_cast< CppModelManagerInterface*>(this));
    return QObject::qt_metacast(_clname);
}

int CPlusPlus::CppModelManagerInterface::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: documentUpdated((*reinterpret_cast< CPlusPlus::Document::Ptr(*)>(_a[1]))); break;
        case 1: sourceFilesRefreshed((*reinterpret_cast< const QStringList(*)>(_a[1]))); break;
        case 2: updateModifiedSourceFiles(); break;
        case 3: { QFuture<void> _r = updateSourceFiles((*reinterpret_cast< const QStringList(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< QFuture<void>*>(_a[0]) = _r; }  break;
        case 4: GC(); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void CPlusPlus::CppModelManagerInterface::documentUpdated(CPlusPlus::Document::Ptr _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void CPlusPlus::CppModelManagerInterface::sourceFilesRefreshed(const QStringList & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
