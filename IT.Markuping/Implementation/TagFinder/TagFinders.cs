using IT.Markuping.Interfaces;
using System;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping.Implementation;

public static class TagFinders
{
    public static readonly OtherSpacesTagFinder<byte> Utf8 = new((MarkupTokens<byte>)MarkupEncodingByteInfos.Utf8.Alphabet);
    public static readonly OtherSpaceTagFinder<byte> Europa = new((MarkupTokens<byte>)MarkupEncodingByteInfos.Europa.Alphabet);
    public static readonly OtherSpacesTagFinder<byte> EBCDIC = new((MarkupTokens<byte>)MarkupEncodingByteInfos.EBCDIC.Alphabet);
    public static readonly OtherSpacesTagFinder<byte> EBCDIC_Turkish = new((MarkupTokens<byte>)MarkupEncodingByteInfos.EBCDIC_Turkish.Alphabet);
    public static readonly OtherSpacesTagFinder<byte> IBM_Latin1 = new((MarkupTokens<byte>)MarkupEncodingByteInfos.IBM_Latin1.Alphabet);

    public static readonly ComplexTagFinder<byte> Utf16 = new(MarkupEncodingByteInfos.Utf16.Alphabet);
    public static readonly ComplexTagFinder<byte> Utf16BE = new(MarkupEncodingByteInfos.Utf16BE.Alphabet);
    public static readonly ComplexTagFinder<byte> Utf32 = new(MarkupEncodingByteInfos.Utf32.Alphabet);
    public static readonly ComplexTagFinder<byte> Utf32BE = new(MarkupEncodingByteInfos.Utf32BE.Alphabet);

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