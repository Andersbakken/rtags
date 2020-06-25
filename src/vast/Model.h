#ifndef MODEL_H
#define MODEL_H

#include <QtCore>
#include "TranslationUnit.h"
#include "Node.h"
#include <memory>

inline QString eatString(CXString str)
{
    const char *cstr = clang_getCString(str);
    QString ret = cstr ? QString::fromLatin1(cstr) : QString();
    clang_disposeString(str);
    return ret;
}

class Model : public QAbstractItemModel
{
public:
    Model(std::unique_ptr<TranslationUnit> &&translationUnit, QObject *parent);

    enum {
        SourceCodeRole = Qt::UserRole
    };

    virtual QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const override;
    virtual QModelIndex parent(const QModelIndex &child) const override;
    virtual int rowCount(const QModelIndex &parent = QModelIndex()) const override;
    virtual int columnCount(const QModelIndex &parent = QModelIndex()) const override;
    virtual bool hasChildren(const QModelIndex &parent = QModelIndex()) const override;
    virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
    virtual QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;

    unsigned int translationUnitFlags() const;
    void setTranslationUnitFlags(unsigned int flags);
private:
    std::unique_ptr<TranslationUnit> mTranslationUnit;
};

#endif
