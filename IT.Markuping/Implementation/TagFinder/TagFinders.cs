using IT.Markuping.Interfaces;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping.Implementation;

public static class TagFinders
{
    public static readonly TagFinderByte Utf8 = TagFinderByte.Utf8;
    public static readonly TagFinderByte Europa = TagFinderByte.Europa;
    public static readonly TagFinderByte EBCDIC = TagFinderByte.EBCDIC;
    public static readonly TagFinderByte EBCDIC_Turkish = TagFinderByte.EBCDIC_Turkish;
    public static readonly TagFinderByte IBM_Latin1 = TagFinderByte.IBM_Latin1;

    public static readonly BytesTagFinder Utf16 = BytesTagFinder.Utf16;
    public static readonly BytesTagFinder Utf16BE = BytesTagFinder.Utf16BE;
    public static readonly BytesTagFinder Utf32 = BytesTagFinder.Utf32;
    public static readonly BytesTagFinder Utf32BE = BytesTagFinder.Utf32BE;

    public static bool TryGet(int codePage, [MaybeNullWhen(false)] out ITagFinder<byte> tagFinder)
    {
        if (TagFinderByte.TryGet(codePage, out var byteTagFinder))
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