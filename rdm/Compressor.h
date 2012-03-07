#ifndef COMPRESSOR_H
#define COMPRESSOR_H

#include <QObject>
#include <QByteArray>

class Compressor : public QObject
{
    Q_OBJECT
public:
    enum Type { Deflate, LZMA, BZIP2 };
    enum Error { NoError, InitError, IOError, CompressError, DecompressError };

    Compressor(Type type = Deflate);

    static void init();

    void compress(const QByteArray& inputfile, const QByteArray& outputfile = QByteArray());
    void decompress(const QByteArray& inputfile, const QByteArray& outputfile = QByteArray());

signals:
    void compressed(const QByteArray& inputfile, const QByteArray& outputfile);
    void decompressed(const QByteArray& inputfile, const QByteArray& outputfile);
    void error(const QByteArray& inputfile, Error error);

private:
    Type m_type;

    friend class CompressorJob;
};

#endif
