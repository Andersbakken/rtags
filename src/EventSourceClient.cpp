#include "EventSourceClient.h"
#include <rct/Log.h>

EventSourceClient::~EventSourceClient()
{
    if (mBuffer)
        free(mBuffer);
}

bool EventSourceClient::connect(const String& host, uint16_t port, const String& path)
{
    mClient.reset(new SocketClient);
    mClient->readyRead().connect(std::bind(&EventSourceClient::onReadyRead, this, std::placeholders::_1, std::placeholders::_2));
    if (!mClient->connect(host, port))
        return false;
    mClient->connected().connect([path](const SocketClient::SharedPtr& client) { client->write(String::format<128>("GET %s HTTP/1.1\r\n\r\n", path.constData())); });
    return true;
}

void EventSourceClient::onReadyRead(const SocketClient::SharedPtr& client, Buffer&& buffer)
{
    Buffer buf = std::move(buffer);
    mBufferLen += buf.size();
    mBuffer = reinterpret_cast<char*>(realloc(mBuffer, mBufferLen + 1));
    memcpy(mBuffer + mBufferLen - buf.size(), buf.data(), buf.size());
    mBuffer[mBufferLen] = '\0';

    char* begin = mBuffer;
    while (char* end = strstr(begin, "\r\n")) {
        if (!mSeenHeaders && end == begin && !strncmp("\r\n", end + 2, 2)) {
            mSeenHeaders = true;
            begin = end + 4;
        } else if (!strncmp(begin, "data:", 5)) {
            String string(begin + 5, end - begin - 5);
            mEvent(string);
        } else if (mSeenHeaders) {
            error() << "invalid EventSource message:" << String(begin, end - begin);
        }
        begin = end + 2;
    }

    if (!*begin) {
        free(mBuffer);
        mBuffer = 0;
        mBufferLen = 0;
    } else if (begin != mBuffer) {
        const size_t newSize = mBufferLen - (begin - mBuffer);
        char* tmp = reinterpret_cast<char*>(malloc(newSize + 1));
        memcpy(tmp, begin, newSize + 1);
        mBufferLen = newSize;
        free(mBuffer);
        mBuffer = tmp;
    }
}
