#include "Window.h"

#include <qaction.h>
#include <qboxlayout.h>
#include <qbytearray.h>
#include <qcheckbox.h>
#include <qdialog.h>
#include <qdialogbuttonbox.h>
#include <qfontdatabase.h>
#include <qheaderview.h>
#include <qitemselectionmodel.h>
#include <qkeysequence.h>
#include <qlabel.h>
#include <qmenu.h>
#include <qmenubar.h>
#include <qnamespace.h>
#include <qobject.h>
#include <qplaintextedit.h>
#include <qsettings.h>
#include <qsizepolicy.h>
#include <qsplitter.h>
#include <qvariant.h>
#include <qwidget.h>
#include <algorithm>
#include <utility>

#include "Model.h"
#include "SearchEdit.h"
#include "TranslationUnit.h"
#include "TreeView.h"

class QMoveEvent;
class QResizeEvent;
class QShowEvent;

Window::Window(std::unique_ptr<TranslationUnit> &&translationUnit)
    : QMainWindow()
{
    QWidget *central = new QWidget(this);
    QVBoxLayout *layout = new QVBoxLayout(central);
    mSplitter = new QSplitter(Qt::Horizontal, central);
    layout->addWidget(mSplitter);
    setCentralWidget(central);
    mTreeView = new TreeView(mSplitter);
    mModel = new Model(std::move(translationUnit), this);
    mTreeView->setModel(mModel);
    mTreeView->header()->setSectionResizeMode(QHeaderView::ResizeToContents);
    mSplitter->addWidget(mTreeView);

    mSourceView = new QPlainTextEdit(mSplitter);
    mSourceView->setReadOnly(true);
    mSourceView->setFont(QFontDatabase::systemFont(QFontDatabase::FixedFont));

    mSplitter->addWidget(mSourceView);
    QObject::connect(mSplitter, SIGNAL(splitterMoved(int, int)), this, SLOT(onSplitterMoved()));
    {
        QByteArray savedState = QSettings().value("splitterState").value<QByteArray>();
        if (!savedState.isEmpty())
            mSplitter->restoreState(savedState);
    }

    QObject::connect(mTreeView->selectionModel(), SIGNAL(currentChanged(QModelIndex, QModelIndex)),
                     this, SLOT(onCurrentChanged(QModelIndex)));

    QWidget *bottom = new QWidget(central);
    bottom->setSizePolicy(bottom->sizePolicy().verticalPolicy(), QSizePolicy::Maximum);
    layout->addWidget(bottom);

    mSearch = new SearchEdit(bottom, mTreeView);
    QHBoxLayout *hbox = new QHBoxLayout(bottom);
    QLabel *buddy = new QLabel("&Search", bottom);
    hbox->addWidget(buddy);
    buddy->setBuddy(mSearch);
    hbox->addWidget(mSearch);
    mSearchLabel = new QLabel(bottom);
    hbox->addWidget(mSearchLabel);
    QObject::connect(mSearch, SIGNAL(returnPressed()), this, SLOT(onSearchReturn()));

    QMenu *file = menuBar()->addMenu("&File");
    file->addAction("&Preferences", this, SLOT(showPreferences()), QKeySequence::Preferences);
    file->addSeparator();
    file->addAction("&Quit", this, SLOT(close()), QKeySequence::Quit);

    QMenu *edit = menuBar()->addMenu("&Edit");
    edit->addAction("&Search", mSearch, SLOT(setFocus()), QKeySequence::Find);
    mFindNext = edit->addAction("&Next result", this, SLOT(searchNext()), QKeySequence::FindNext);
    mFindPrevious = edit->addAction("&Previous result", this, SLOT(searchPrevious()), QKeySequence::FindPrevious);
}

void Window::onSearchReturn()
{
    if (mSearch->text().isEmpty()) {
        mFindNext->setEnabled(false);
        mFindPrevious->setEnabled(false);
        return;
    }
    if (mSearch->text() != mLastSearch) {
        mLastSearch = mSearch->text();
        mMatches.clear();
        mLastSearchIndex = 0;
        mSearchLabel->clear();
        QModelIndex parent = mTreeView->selectedIndex().parent();
        if (!parent.isValid()) {
            parent = mModel->index(0, 0);
        }
        const int rows = mModel->rowCount(parent);
        const int columns = mModel->columnCount(parent);
        for (int row=0; row<rows; ++row) {
            for (int column=0; column<columns; ++column) {
                const QModelIndex idx = mModel->index(row, column, parent);
                if (idx.data().toString().contains(mLastSearch)) {
                    mMatches.push_back(mModel->index(row, 0, parent));
                    // qDebug() << "got a match" << idx.data().toString()
                    //          << mLastSearch;
                    break;
                }
            }
        }
        if (mMatches.size()) {
            mFindNext->setEnabled(true);
            mFindPrevious->setEnabled(true);
            mSearchLabel->setText("1/" + QString::number(mMatches.size()));
            mTreeView->setCurrentIndex(mMatches[0]);
        } else {
            mFindNext->setEnabled(false);
            mFindPrevious->setEnabled(false);
            mSearchLabel->setText("No matches");
        }
        return;
    }

    searchNext();
}

void Window::searchNext()
{
    if (mMatches.size() <= 1)
        return;
    if (++mLastSearchIndex == mMatches.size()) {
        mLastSearchIndex = 0;
    }
    mSearchLabel->setText(QString::number(mLastSearchIndex + 1) + "/" + QString::number(mMatches.size()));
    mTreeView->setCurrentIndex(mMatches[mLastSearchIndex]);

}

void Window::searchPrevious()
{
    if (mMatches.size() <= 1)
        return;
    if (!mLastSearchIndex--)
        mLastSearchIndex = mMatches.size() - 1;
    mSearchLabel->setText(QString::number(mLastSearchIndex + 1) + "/" + QString::number(mMatches.size()));
    mTreeView->setCurrentIndex(mMatches[mLastSearchIndex]);
}

void Window::showPreferences()
{
    QDialog dialog(this);
    QVBoxLayout *layout = new QVBoxLayout(&dialog);
    QSettings settings;
    QCheckBox *showDefines = new QCheckBox("Show defines", &dialog);
    showDefines->setChecked(settings.value("showDefines").value<bool>());
    QCheckBox *showIncludes = new QCheckBox("Show includes", &dialog);
    showIncludes->setChecked(settings.value("showIncludes").value<bool>());
    QCheckBox *showTypedefs = new QCheckBox("Show typedefs", &dialog);
    showTypedefs->setChecked(settings.value("showTypedefs").value<bool>());

    layout->addWidget(showDefines);
    layout->addWidget(showIncludes);
    layout->addWidget(showTypedefs);
    QDialogButtonBox *buttons = new QDialogButtonBox(QDialogButtonBox::Save|QDialogButtonBox::Cancel, &dialog);
    layout->addWidget(buttons);
    QObject::connect(buttons, SIGNAL(accepted()), &dialog, SLOT(accept()));
    QObject::connect(buttons, SIGNAL(rejected()), &dialog, SLOT(reject()));
    if (dialog.exec()) {
        settings.setValue("showDefines", showDefines->isChecked());
        settings.setValue("showIncludes", showIncludes->isChecked());
        settings.setValue("showTypeDefs", showTypedefs->isChecked());
        unsigned int flags = 0;
        if (showDefines->isChecked())
            flags |= TranslationUnit::ShowDefines;
        if (showIncludes->isChecked())
            flags |= TranslationUnit::ShowIncludes;
        if (showTypedefs->isChecked())
            flags |= TranslationUnit::ShowTypedefs;

        mModel->setTranslationUnitFlags(flags);

    }
}

void Window::onSplitterMoved()
{
    QSettings().setValue("splitterState", mSplitter->saveState());
    if ((mSourceView->width() != 0) != mSourceViewWasVisible) {
        mSourceViewWasVisible = !mSourceViewWasVisible;
        if (mSourceViewWasVisible)
            onCurrentChanged(mTreeView->selectedIndex());
    }
}

void Window::onCurrentChanged(const QModelIndex &index)
{
    if (mSourceViewWasVisible)
        mSourceView->setPlainText(index.data(Model::SourceCodeRole).value<QString>());
}

void Window::showEvent(QShowEvent *e)
{
    QMainWindow::showEvent(e);
    mSourceViewWasVisible = mSourceView->width() != 0;
    QByteArray geom = QSettings().value("geometry").value<QByteArray>();
    if (!geom.isEmpty())
        restoreGeometry(geom);
}

void Window::resizeEvent(QResizeEvent *e)
{
    QMainWindow::resizeEvent(e);
    QSettings().setValue("geometry", saveGeometry());
}

void Window::moveEvent(QMoveEvent *e)
{
    QMainWindow::moveEvent(e);
    QSettings().setValue("geometry", saveGeometry());
}
