#ifndef TREEVIEW_H
#define TREEVIEW_H

#include <QtWidgets>

class TreeView : public QTreeView
{
public:
    TreeView(QWidget *parent)
        : QTreeView(parent)
    {}
    QModelIndex selectedIndex() const
    {
        return selectedIndexes().value(0);
    }
    void keyPressEvent(QKeyEvent *e)
    {
        QTreeView::keyPressEvent(e);
    }
    void keyReleaseEvent(QKeyEvent *e)
    {
        QTreeView::keyReleaseEvent(e);
    }

};

#endif /* TREEVIEW_H */
