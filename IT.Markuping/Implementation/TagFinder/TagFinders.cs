using IT.Markuping.Interfaces;
using System;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping.Implementation;

public static class TagFinders
{
    public static readonly OtherSpacesTagFinder<byte> Utf8 = new((MarkupTokens<byte>)MarkupAlphabets.Byte.Utf8);
    public static readonly OtherSpaceTagFinder<byte> Europa = new((MarkupTokens<byte>)MarkupAlphabets.Byte.Europa);
    public static readonly OtherSpacesTagFinder<byte> EBCDIC = new((MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC);
    public static readonly OtherSpacesTagFinder<byte> EBCDIC_Turkish = new((MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC_Turkish);
    public static readonly OtherSpacesTagFinder<byte> IBM_Latin1 = new((MarkupTokens<byte>)MarkupAlphabets.Byte.IBM_Latin1);

    public static readonly ComplexTagFinder<byte> Utf16 = new(MarkupAlphabets.Byte.Utf16);
    public static readonly ComplexTagFinder<byte> Utf16BE = new(MarkupAlphabets.Byte.Utf16BE);
    public static readonly ComplexTagFinder<byte> Utf32 = new(MarkupAlphabets.Byte.Utf32);
    public static readonly ComplexTagFinder<byte> Utf32BE = new(MarkupAlphabets.Byte.Utf32BE);

    public static bool TryGet(int codePage, [MaybeNullWhen(false)] out ITagFinder<byte> tagFinder)
    {
        if (MarkupEncodingByteInfos.Utf8.CodePages.IndexOf(codePage) > -1)
        {
            tagFinder = Utf8;
            return true;
        }
        if (codePage == 29001)
        {
            tagFinder = Europa;
            return true;
        }
        if (MarkupEncodingByteInfos.EBCDIC.CodePages.IndexOf(codePage) > -1)
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
}