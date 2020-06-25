#ifndef SEARCHEDIT_H
#define SEARCHEDIT_H

#include <QtWidgets>
#include "TreeView.h"

class SearchEdit : public QLineEdit
{
public:
    SearchEdit(QWidget *parent, TreeView *treeView)
        : QLineEdit(parent), mTreeView(treeView)
    {}

    void keyPressEvent(QKeyEvent *e)
    {
        switch (e->key()) {
        case Qt::Key_Up:
        case Qt::Key_Down:
        case Qt::Key_PageUp:
        case Qt::Key_PageDown:
            mTreeView->setFocus();
            mTreeView->keyPressEvent(e);
            return;
        }
        QLineEdit::keyPressEvent(e);
    }

    void keyReleaseEvent(QKeyEvent *e)
    {
        switch (e->key()) {
        case Qt::Key_Up:
        case Qt::Key_Down:
        case Qt::Key_PageUp:
        case Qt::Key_PageDown:
            mTreeView->setFocus();
            mTreeView->keyReleaseEvent(e);
            return;
        }
        QLineEdit::keyReleaseEvent(e);
    }
private:
    TreeView *mTreeView;
};


#endif /* SEARCHEDIT_H */
