using IT.Markuping.Interfaces;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping.Implementation;

public static class TagFinders
{
    public static readonly ByteTagFinder Utf8 = ByteTagFinder.Utf8;
    public static readonly ByteTagFinder Europa = ByteTagFinder.Europa;
    public static readonly ByteTagFinder EBCDIC = ByteTagFinder.EBCDIC;
    public static readonly ByteTagFinder EBCDIC_Turkish = ByteTagFinder.EBCDIC_Turkish;
    public static readonly ByteTagFinder IBM_Latin1 = ByteTagFinder.IBM_Latin1;

    public static readonly BytesTagFinder Utf16 = BytesTagFinder.Utf16;
    public static readonly BytesTagFinder Utf16BE = BytesTagFinder.Utf16BE;
    public static readonly BytesTagFinder Utf32 = BytesTagFinder.Utf32;
    public static readonly BytesTagFinder Utf32BE = BytesTagFinder.Utf32BE;

    public static bool TryGet(int codePage, [MaybeNullWhen(false)] out ITagFinder<byte> tagFinder)
    {
        if (ByteTagFinder.TryGet(codePage, out var byteTagFinder))
        {
            tagFinder = byteTagFinder;
            return true;
        }

        if (BytesTagFinder.TryGet(codePage, out var tagFinderBytes))
        {
            tagFinder = tagFinderBytes;
            return true;
        }

        tagFinder = null;
        return false;
    }
}