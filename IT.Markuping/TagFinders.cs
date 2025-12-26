using IT.Markuping.Implementation;
using IT.Markuping.Interfaces;
using IT.Markuping.Internal;
using System;

namespace IT.Markuping;

public static class TagFinders
{
    public static readonly TagFinderByte Utf8 = TagFinderByte.Utf8;
    public static readonly TagFinderByte Europa = TagFinderByte.Europa;
    public static readonly TagFinderByte EBCDIC = TagFinderByte.EBCDIC;
    public static readonly TagFinderByte EBCDIC_Turkish = TagFinderByte.EBCDIC_Turkish;
    public static readonly TagFinderByte IBM_Latin1 = TagFinderByte.IBM_Latin1;

    public static readonly OtherSpacesTagFinder<byte> OtherSpaces_Utf8 = new((MarkupTokens<byte>)MarkupAlphabets.Byte.Utf8);
    public static readonly OtherSpaceTagFinder<byte> OtherSpace_Europa = new((MarkupTokens<byte>)MarkupAlphabets.Byte.Europa);
    public static readonly OtherSpacesTagFinder<byte> OtherSpaces_EBCDIC = new((MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC);
    public static readonly OtherSpacesTagFinder<byte> OtherSpaces_EBCDIC_Turkish = new((MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC_Turkish);
    public static readonly OtherSpacesTagFinder<byte> OtherSpaces_IBM_Latin1 = new((MarkupTokens<byte>)MarkupAlphabets.Byte.IBM_Latin1);

    public static readonly ProxyTagFinderByte<char> Utf16 = new(TagFinderChar.Utf16);
    public static readonly ProxyTagFinderByte<char> Utf16BE = new(TagFinderChar.Utf16BE);
    public static readonly ProxyTagFinderByte<int> Utf32 = new(TagFinderInt32.Utf32);
    public static readonly ProxyTagFinderByte<int> Utf32BE = new(TagFinderInt32.Utf32BE);

    public static readonly ComplexTagFinder<byte> Complex_Utf16 = new(MarkupAlphabets.Byte.Utf16);
    public static readonly ComplexTagFinder<byte> Complex_Utf16BE = new(MarkupAlphabets.Byte.Utf16BE);
    public static readonly ComplexTagFinder<byte> Complex_Utf32 = new(MarkupAlphabets.Byte.Utf32);
    public static readonly ComplexTagFinder<byte> Complex_Utf32BE = new(MarkupAlphabets.Byte.Utf32BE);

    public static bool TryGet(int codePage,
#if NETSTANDARD2_0
        out ITagFinder<byte>? tagFinder
#else
        [System.Diagnostics.CodeAnalysis.MaybeNullWhen(false)]
        out ITagFinder<byte> tagFinder
#endif
    )
    {
        if (MarkupCodePages.Utf8.AsSpan().IndexOf(codePage) > -1)
        {
            tagFinder = Utf8;
            return true;
        }
        if (codePage == 29001)
        {
            tagFinder = Europa;
            return true;
        }
        if (MarkupCodePages.EBCDIC.AsSpan().IndexOf(codePage) > -1)
        {
            tagFinder = EBCDIC;
            return true;
        }
        if (codePage == 1026 || codePage == 20905)
        {
            tagFinder = EBCDIC_Turkish;
            return true;
        }
        if (codePage == 1047 || codePage == 20924)
        {
            tagFinder = IBM_Latin1;
            return true;
        }
        if (codePage == 1200)
        {
            tagFinder = Utf16;
            return true;
        }
        if (codePage == 1201)
        {
            tagFinder = Utf16BE;
            return true;
        }
        if (codePage == 12000)
        {
            tagFinder = Utf32;
            return true;
        }
        if (codePage == 12001)
        {
            tagFinder = Utf32BE;
            return true;
        }
        tagFinder = null;
        return false;
    }

    /*
     public static bool TryGetStrict(int codePage, [MaybeNullWhen(false)] out ITagFinder<byte> tagFinder)
    {
        if (Utf8_Strict.CodePages.IndexOf(codePage) > -1)
        {
            tagFinder = Utf8_Strict;
            return true;
        }
        if (EBCDIC_Strict.CodePages.IndexOf(codePage) > -1)
        {
            tagFinder = EBCDIC_Strict;
            return true;
        }
        if (codePage == 1026 || codePage == 20905)
        {
            tagFinder = EBCDIC_Turkish_Strict;
            return true;
        }
        if (codePage == 1200)
        {
            tagFinder = Utf16_Strict;
            return true;
        }
        if (codePage == 1201)
        {
            tagFinder = Utf16BE_Strict;
            return true;
        }
        if (codePage == 12000)
        {
            tagFinder = Utf32_Strict;
            return true;
        }
        if (codePage == 12001)
        {
            tagFinder = Utf32BE_Strict;
            return true;
        }
        tagFinder = null;
        return false;
    }
     */
}