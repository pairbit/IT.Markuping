using IT.Markuping.Interfaces;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping.Implementation;

public static class OldTagFinders
{
    public static readonly TagFinderByte Utf8 = TagFinderByte.Utf8;
    public static readonly TagFinderByte Europa = TagFinderByte.Europa;
    public static readonly TagFinderByte EBCDIC = TagFinderByte.EBCDIC;
    public static readonly TagFinderByte EBCDIC_Turkish = TagFinderByte.EBCDIC_Turkish;
    public static readonly TagFinderByte IBM_Latin1 = TagFinderByte.IBM_Latin1;

    public static readonly ComplexTagFinderByte Utf16 = ComplexTagFinderByte.Utf16;
    public static readonly ComplexTagFinderByte Utf16BE = ComplexTagFinderByte.Utf16BE;
    public static readonly ComplexTagFinderByte Utf32 = ComplexTagFinderByte.Utf32;
    public static readonly ComplexTagFinderByte Utf32BE = ComplexTagFinderByte.Utf32BE;

    public static bool TryGet(int codePage, [MaybeNullWhen(false)] out ITagFinder<byte> tagFinder)
    {
        if (TagFinderByte.TryGet(codePage, out var tagFinderByte))
        {
            tagFinder = tagFinderByte;
            return true;
        }

        if (ComplexTagFinderByte.TryGet(codePage, out var complexTagFinderByte))
        {
            tagFinder = complexTagFinderByte;
            return true;
        }

        tagFinder = null;
        return false;
    }
}