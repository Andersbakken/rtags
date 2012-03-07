#include "Compressor.h"
#include <QFile>
#include <QRunnable>
#include <QThreadPool>
#include <QMetaType>
#include <zlib.h>
#include <lzma.h>

// Gah, the things we do to keep moc happy...
typedef Compressor::Error Error;

class CompressorJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    enum Mode { Compress, Decompress };

    CompressorJob(Compressor* c);

    void setArgs(Mode m, const QByteArray& in, const QByteArray& out);

    static CompressorJob* create(Compressor* c);
    static QByteArray extension(Compressor::Type type);

signals:
    void compressed(const QByteArray& inputfile, const QByteArray& outputfile);
    void decompressed(const QByteArray& inputfile, const QByteArray& outputfile);
    void error(const QByteArray& inputfile, Error err);

protected:
    QByteArray realFile(const QByteArray& file);

    Compressor* compressor;
    Mode mode;
    QByteArray input, output;
};

CompressorJob::CompressorJob(Compressor* c)
    : QObject(0), compressor(c), mode(Compress)
{
}

void CompressorJob::setArgs(Mode m, const QByteArray& in, const QByteArray& out)
{
    mode = m;
    input = in;
    output = out;
}

QByteArray CompressorJob::realFile(const QByteArray& file)
{
    const QByteArray ext = extension(compressor->m_type);
    if (file.endsWith(ext))
        return file.left(file.size() - ext.size());
    return file + ext;
}

class CompressorZlib : public CompressorJob
{
public:
    enum { BufferSize = 8192 };

    CompressorZlib(Compressor* c);

    void run();

private:
    void compress();
    void decompress();
};

class CompressorLzma : public CompressorJob
{
public:
    enum { BufferSize = 8192 };

    CompressorLzma(Compressor* c);

    void run();

private:
    void compress();
    void decompress();
};

#include "Compressor.moc"

CompressorZlib::CompressorZlib(Compressor* c)
    : CompressorJob(c)
{
}

void CompressorZlib::run()
{
    if (mode == Compress)
        compress();
    else
        decompress();
}

void CompressorZlib::compress()
{
    if (output.isEmpty())
        output = realFile(input);

    QFile infile(input);
    if (!infile.open(QFile::ReadOnly)) {
        emit error(input, Compressor::IOError);
        emit compressed(input, QByteArray());
        return;
    }
    QFile outfile(output);
    if (!outfile.open(QFile::WriteOnly | QFile::Truncate)) {
        infile.close();
        emit error(input, Compressor::IOError);
        emit compressed(input, QByteArray());
        return;
    }

    z_stream strm;
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    int ret = deflateInit(&strm, -1);
    if (ret != Z_OK) {
        infile.close();
        outfile.close();
        emit error(input, Compressor::InitError);
        emit compressed(input, QByteArray());
        return;
    }

    Error err = Compressor::NoError;

    int flush;
    unsigned int have;
    char inbuf[BufferSize], outbuf[BufferSize];
    do {
        strm.avail_in = infile.read(inbuf, BufferSize);
        if (infile.error() != QFile::NoError) {
            err = Compressor::IOError;
            break;
        }
        flush = infile.atEnd() ? Z_FINISH : Z_NO_FLUSH;
        strm.next_in = reinterpret_cast<unsigned char*>(inbuf);
        do {
            strm.avail_out = BufferSize;
            strm.next_out = reinterpret_cast<unsigned char*>(outbuf);
            ret = deflate(&strm, flush);
            if (ret == Z_STREAM_ERROR) {
                err = Compressor::CompressError;
                break;
            }
            have = BufferSize - strm.avail_out;
            outfile.write(outbuf, have);
            if (outfile.error() != QFile::NoError) {
                err = Compressor::IOError;
                break;
            }
        } while (strm.avail_out == 0);
        if (strm.avail_in != 0)
            err = Compressor::CompressError;
    } while (flush != Z_FINISH && err == Compressor::NoError);
    if (ret != Z_STREAM_END)
        err = Compressor::CompressError;

    deflateEnd(&strm);
    infile.close();
    outfile.close();

    if (err != Compressor::NoError) {
        QFile::remove(output);
        emit error(input, err);
        emit compressed(input, QByteArray());
    } else
        emit compressed(input, output);
}

void CompressorZlib::decompress()
{
    if (output.isEmpty())
        output = realFile(input);

    QFile infile(input);
    if (!infile.open(QFile::ReadOnly)) {
        emit error(input, Compressor::IOError);
        emit decompressed(input, QByteArray());
        return;
    }
    QFile outfile(output);
    if (!outfile.open(QFile::WriteOnly | QFile::Truncate)) {
        infile.close();
        emit error(input, Compressor::IOError);
        emit decompressed(input, QByteArray());
        return;
    }

    z_stream strm;
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    strm.avail_in = 0;
    strm.next_in = Z_NULL;
    int ret = inflateInit(&strm);

    if (ret != Z_OK) {
        infile.close();
        outfile.close();
        emit error(input, Compressor::InitError);
        emit decompressed(input, QByteArray());
        return;
    }

    Error err = Compressor::NoError;

    unsigned int have;
    char inbuf[BufferSize], outbuf[BufferSize];
    do {
        strm.avail_in = infile.read(inbuf, BufferSize);
        if (infile.error() != QFile::NoError) {
            err = Compressor::IOError;
            break;
        }
        if (strm.avail_in == 0)
            break;
        strm.next_in = reinterpret_cast<unsigned char*>(inbuf);
        do {
            strm.avail_out = BufferSize;
            strm.next_out = reinterpret_cast<unsigned char*>(outbuf);
            ret = inflate(&strm, Z_NO_FLUSH);
            switch (ret) {
            case Z_NEED_DICT:
            case Z_DATA_ERROR:
            case Z_MEM_ERROR:
                err = Compressor::DecompressError;
                break;
            default:
                break;
            }
            if (err != Compressor::NoError)
                break;
            have = BufferSize - strm.avail_out;
            outfile.write(outbuf, have);
            if (outfile.error() != QFile::NoError) {
                err = Compressor::IOError;
                break;
            }
        } while (strm.avail_out == 0);
    } while (ret != Z_STREAM_END && err == Compressor::NoError);

    inflateEnd(&strm);
    infile.close();
    outfile.close();

    if (err != Compressor::NoError) {
        QFile::remove(output);
        emit error(input, err);
        emit decompressed(input, QByteArray());
    } else
        emit decompressed(input, output);
}

CompressorLzma::CompressorLzma(Compressor* c)
    : CompressorJob(c)
{
}

void CompressorLzma::run()
{
    if (mode == Compress)
        compress();
    else
        decompress();
}

void CompressorLzma::compress()
{
    if (output.isEmpty())
        output = realFile(input);

    QFile infile(input);
    if (!infile.open(QFile::ReadOnly)) {
        emit error(input, Compressor::IOError);
        emit compressed(input, QByteArray());
        return;
    }
    QFile outfile(output);
    if (!outfile.open(QFile::WriteOnly | QFile::Truncate)) {
        infile.close();
        emit error(input, Compressor::IOError);
        emit compressed(input, QByteArray());
        return;
    }

    const uint32_t level = 6;
    const lzma_check check = LZMA_CHECK_CRC64;
    lzma_stream strm = LZMA_STREAM_INIT;
    char inbuf[BufferSize], outbuf[BufferSize];
    size_t inlen, outlen;
    lzma_action action;
    lzma_ret ret;

    ret = lzma_easy_encoder(&strm, level, check);
    if (ret != LZMA_OK) {
        infile.close();
        outfile.close();
        emit error(input, Compressor::InitError);
        emit compressed(input, QByteArray());
        return;
    }

    Error err = Compressor::NoError;

    do {
        inlen = infile.read(inbuf, BufferSize);
        if (infile.error() != QFile::NoError) {
            err = Compressor::IOError;
            break;
        }
        strm.next_in = reinterpret_cast<unsigned char*>(inbuf);
        strm.avail_in = inlen;
        action = infile.atEnd() ? LZMA_FINISH : LZMA_RUN;
        do {
            strm.next_out = reinterpret_cast<unsigned char*>(outbuf);
            strm.avail_out = BufferSize;
            ret = lzma_code(&strm, action);
            if (ret != LZMA_OK && ret != LZMA_STREAM_END) {
                err = Compressor::CompressError;
                break;
            }
            outlen = BufferSize - strm.avail_out;
            outfile.write(outbuf, outlen);
            if (outfile.error() != QFile::NoError) {
                err = Compressor::IOError;
                break;
            }
        } while (strm.avail_out == 0);
    } while (ret != LZMA_STREAM_END && err == Compressor::NoError);

    lzma_end(&strm);
    infile.close();
    outfile.close();

    if (err != Compressor::NoError) {
        QFile::remove(output);
        emit error(input, err);
        emit compressed(input, QByteArray());
    } else
        emit compressed(input, output);
}

void CompressorLzma::decompress()
{
    if (output.isEmpty())
        output = realFile(input);

    QFile infile(input);
    if (!infile.open(QFile::ReadOnly)) {
        emit error(input, Compressor::IOError);
        emit decompressed(input, QByteArray());
        return;
    }
    QFile outfile(output);
    if (!outfile.open(QFile::WriteOnly | QFile::Truncate)) {
        infile.close();
        emit error(input, Compressor::IOError);
        emit decompressed(input, QByteArray());
        return;
    }

    lzma_stream strm = LZMA_STREAM_INIT;
    const uint32_t flags = LZMA_TELL_UNSUPPORTED_CHECK | LZMA_CONCATENATED;
    const uint64_t limit = UINT64_MAX;
    char inbuf[BufferSize], outbuf[BufferSize];
    size_t inlen, outlen;
    lzma_action action;
    lzma_ret ret;

    ret = lzma_stream_decoder(&strm, limit, flags);
    if (ret != LZMA_OK) {
        infile.close();
        outfile.close();
        emit error(input, Compressor::InitError);
        emit decompressed(input, QByteArray());
        return;
    }

    Error err = Compressor::NoError;

    do {
        inlen = infile.read(inbuf, BufferSize);
        if (infile.error() != QFile::NoError) {
            err = Compressor::IOError;
            break;
        }
        strm.next_in = reinterpret_cast<unsigned char*>(inbuf);
        strm.avail_in = inlen;

        action = infile.atEnd() ? LZMA_FINISH : LZMA_RUN;
        do {
            strm.next_out = reinterpret_cast<unsigned char*>(outbuf);
            strm.avail_out = BufferSize;

            ret = lzma_code(&strm, action);

            if (ret != LZMA_OK && ret != LZMA_STREAM_END) {
                err = Compressor::DecompressError;
                break;
            }

            outlen = BufferSize - strm.avail_out;
            outfile.write(outbuf, outlen);
            if (outfile.error() != QFile::NoError) {
                err = Compressor::IOError;
                break;
            }
        } while (strm.avail_out == 0);
    } while (ret != LZMA_STREAM_END && err == Compressor::NoError);

    lzma_end(&strm);
    infile.close();
    outfile.close();

    if (err != Compressor::NoError) {
        QFile::remove(output);
        emit error(input, err);
        emit decompressed(input, QByteArray());
    } else
        emit decompressed(input, output);
}

CompressorJob* CompressorJob::create(Compressor* c)
{
    CompressorJob* impl = 0;
    switch (c->m_type) {
    case Compressor::Deflate:
        impl = new CompressorZlib(c);
        break;
    case Compressor::LZMA:
        impl = new CompressorLzma(c);
        break;
    default:
        qWarning("Unknown compressor type: %d", c->m_type);
        break;
    }
    return impl;
}

QByteArray CompressorJob::extension(Compressor::Type type)
{
    QByteArray ext;
    switch (type) {
    case Compressor::Deflate:
        ext = ".gz";
        break;
    case Compressor::LZMA:
        ext = ".lzma";
        break;
    case Compressor::BZIP2:
        ext = ".bz2";
        break;
    }
    return ext;
}

Compressor::Compressor(Type type)
    : m_type(type)
{
}

void Compressor::init()
{
    qRegisterMetaType<Error>("Error");
}

void Compressor::compress(const QByteArray& inputfile, const QByteArray& outputfile)
{
    CompressorJob* job = CompressorJob::create(this);
    if (!job) {
        emit error(inputfile, Compressor::InitError);
        emit compressed(inputfile, QByteArray());
        return;
    }

    connect(job, SIGNAL(compressed(QByteArray, QByteArray)),
            this, SIGNAL(compressed(QByteArray, QByteArray)), Qt::QueuedConnection);
    connect(job, SIGNAL(error(QByteArray, Error)),
            this, SIGNAL(error(QByteArray, Error)), Qt::QueuedConnection);

    job->setArgs(CompressorJob::Compress, inputfile, outputfile);
    QThreadPool::globalInstance()->start(job);
}

void Compressor::decompress(const QByteArray& inputfile, const QByteArray& outputfile)
{
    CompressorJob* job = CompressorJob::create(this);
    if (!job) {
        emit error(inputfile, Compressor::InitError);
        emit compressed(inputfile, QByteArray());
        return;
    }

    connect(job, SIGNAL(decompressed(QByteArray, QByteArray)),
            this, SIGNAL(decompressed(QByteArray, QByteArray)), Qt::QueuedConnection);
    connect(job, SIGNAL(error(QByteArray, Error)),
            this, SIGNAL(error(QByteArray, Error)), Qt::QueuedConnection);

    job->setArgs(CompressorJob::Decompress, inputfile, outputfile);
    QThreadPool::globalInstance()->start(job);
}
