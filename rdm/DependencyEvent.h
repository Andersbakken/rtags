#ifndef DependencyEvent_h
#define DependencyEvent_h

#include "Indexer.h"
#include <QEvent>

class DependencyEvent : public QEvent
{
public:
    enum { Type = QEvent::User + 1 };

    DependencyEvent(const DependencyHash& d)
        : QEvent(static_cast<QEvent::Type>(Type)), deps(d)
    {
    }

    DependencyHash deps;
};

#endif
