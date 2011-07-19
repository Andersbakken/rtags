/****************************************************************************
** Meta object code from reading C++ file 'OverviewModel.h'
**
** Created: Mon Jul 18 22:51:58 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.4)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "OverviewModel.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'OverviewModel.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.4. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_CPlusPlus__OverviewModel[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      30,   26,   25,   25, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_CPlusPlus__OverviewModel[] = {
    "CPlusPlus::OverviewModel\0\0doc\0"
    "rebuild(CPlusPlus::Document::Ptr)\0"
};

const QMetaObject CPlusPlus::OverviewModel::staticMetaObject = {
    { &QAbstractItemModel::staticMetaObject, qt_meta_stringdata_CPlusPlus__OverviewModel,
      qt_meta_data_CPlusPlus__OverviewModel, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &CPlusPlus::OverviewModel::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *CPlusPlus::OverviewModel::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *CPlusPlus::OverviewModel::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_CPlusPlus__OverviewModel))
        return static_cast<void*>(const_cast< OverviewModel*>(this));
    return QAbstractItemModel::qt_metacast(_clname);
}

int CPlusPlus::OverviewModel::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QAbstractItemModel::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: rebuild((*reinterpret_cast< CPlusPlus::Document::Ptr(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
