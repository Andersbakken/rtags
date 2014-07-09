#ifndef DataFile_h
#define DataFile_h

/* This file is part of RTags.

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

class DataFile
{
public:
    DataFile(const Path &path)
        : mFile(0), mSizeOffset(-1), mSerializer(0), mDeserializer(0), mPath(path)
    {}

    ~DataFile()
    {
        delete mDeserializer;
        if (mFile)
            flush();
    }

    bool flush()
    {
        const int size = ftell(mFile);
        fseek(mFile, mSizeOffset, SEEK_SET);
        operator<<(size);

        fclose(mFile);
        mFile = 0;
        delete mSerializer;
        mSerializer = 0;
        if (rename(mTempFilePath.constData(), mPath.constData())) {
            Path::rm(mTempFilePath);
            mError = String::format<128>("rename error: %d %s", errno, strerror(errno));
            return false;
        }
        return true;
    }

    enum Mode {
        Read,
        Write
    };
    String error() const { return mError; }
    bool open(Mode mode)
    {
        assert(!mFile);
        if (mode == Write) {
            if (!Path::mkdir(mPath.parentDir()))
                return false;
            mTempFilePath = mPath + "XXXXXX";
            const int ret = mkstemp(&mTempFilePath[0]);
            if (ret == -1) {
                mError = String::format<128>("mkstemp failure %d (%s)", errno, strerror(errno));
                return false;
            }
            mFile = fdopen(ret, "w");
            if (!mFile) {
                mError = String::format<128>("fdopen failure %d (%s)", errno, strerror(errno));
                close(ret);
                return false;
            }
            mSerializer = new Serializer(mFile);
            operator<<(static_cast<int>(RTags::DatabaseVersion));
            mSizeOffset = ftell(mFile);
            operator<<(static_cast<int>(0));
            return true;
        } else {
            mContents = mPath.readAll();
            if (mContents.isEmpty()) {
                if (mPath.exists())
                    mError = "Read error " + mPath;
                return false;
            }
            mDeserializer = new Deserializer(mContents);
            int version;
            (*mDeserializer) >> version;
            if (version != RTags::DatabaseVersion) {
                mError = String::format<128>("Wrong database version. Expected %d, got %d for %s.", RTags::DatabaseVersion, version, mPath.constData());
                return false;
            }
            int fs;
            (*mDeserializer) >> fs;
            if (fs != mContents.size()) {
                mError = String::format<128>("%s seems to be corrupted. Size should have been %d but was %d",
                                             mPath.constData(), mContents.size(), fs);
                return false;
            }
            return true;
        }
    }

    template <typename T> DataFile &operator<<(const T &t)
    {
        assert(mSerializer);
        (*mSerializer) << t;
        return *this;
    }
    template <typename T> DataFile &operator>>(T &t)
    {
        assert(mDeserializer);
        (*mDeserializer) >> t;
        return *this;
    }
private:
    FILE *mFile;
    int mSizeOffset;
    Serializer *mSerializer;
    Deserializer *mDeserializer;
    Path mPath, mTempFilePath;
    String mContents;
    String mError;
};
#endif
