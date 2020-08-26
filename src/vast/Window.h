#ifndef WINDOW_H
#define WINDOW_H

#include <qabstractitemmodel.h>
#include <qmainwindow.h>
#include <qobjectdefs.h>
#include <qstring.h>
#include <stddef.h>
#include <QtWidgets>
#include <memory>
#include <vector>

#include "TranslationUnit.h"
#include "Model.h"
#include "TreeView.h"
#include "SearchEdit.h"

class Model;
class QAction;
class QLabel;
class QMoveEvent;
class QObject;
class QPlainTextEdit;
class QResizeEvent;
class QShowEvent;
class QSplitter;
class SearchEdit;
class TranslationUnit;
class TreeView;

class Window : public QMainWindow
{
    Q_OBJECT
public:
    Window(std::unique_ptr<TranslationUnit> &&translationUnit);
protected:
    virtual void showEvent(QShowEvent *e) override;
    virtual void resizeEvent(QResizeEvent *e) override;
    virtual void moveEvent(QMoveEvent *e) override;
private:
    Q_INVOKABLE void onSearchReturn();
    Q_INVOKABLE void searchNext();
    Q_INVOKABLE void searchPrevious();
    Q_INVOKABLE void showPreferences();
    Q_INVOKABLE void onSplitterMoved();
    Q_INVOKABLE void onCurrentChanged(const QModelIndex &index);
private:
    QSplitter *mSplitter;
    TreeView *mTreeView;
    SearchEdit *mSearch;
    QLabel *mSearchLabel;
    Model *mModel;
    QPlainTextEdit *mSourceView;
    bool mSourceViewWasVisible;

    QAction *mFindNext, *mFindPrevious;
    QString mLastSearch;
    std::vector<QModelIndex> mMatches;
    size_t mLastSearchIndex { 0 };
};


#endif /* WINDOW_H */
