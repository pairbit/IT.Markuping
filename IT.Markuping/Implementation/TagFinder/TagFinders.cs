using IT.Markuping.Interfaces;
using System;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping.Implementation;

public static class TagFinders
{
    public static readonly OtherSpacesTagFinder<byte> Utf8 = new((MarkupEncodingTokens<byte>)MarkupEncodingInfos.Utf8.Encoding);
    public static readonly OtherSpaceTagFinder<byte> Europa = new((MarkupEncodingTokens<byte>)MarkupEncodingInfos.Europa.Encoding);
    public static readonly OtherSpacesTagFinder<byte> EBCDIC = new((MarkupEncodingTokens<byte>)MarkupEncodingInfos.EBCDIC.Encoding);
    public static readonly OtherSpacesTagFinder<byte> EBCDIC_Turkish = new((MarkupEncodingTokens<byte>)MarkupEncodingInfos.EBCDIC_Turkish.Encoding);
    public static readonly OtherSpacesTagFinder<byte> IBM_Latin1 = new((MarkupEncodingTokens<byte>)MarkupEncodingInfos.IBM_Latin1.Encoding);

    public static readonly ComplexTagFinder<byte> Utf16 = new(MarkupEncodingInfos.Utf16.Encoding);
    public static readonly ComplexTagFinder<byte> Utf16BE = new(MarkupEncodingInfos.Utf16BE.Encoding);
    public static readonly ComplexTagFinder<byte> Utf32 = new(MarkupEncodingInfos.Utf32.Encoding);
    public static readonly ComplexTagFinder<byte> Utf32BE = new(MarkupEncodingInfos.Utf32BE.Encoding);

    public static bool TryGet(int codePage, [MaybeNullWhen(false)] out ITagFinder<byte> tagFinder)
    {
        if (MarkupEncodingInfos.Utf8.CodePages.IndexOf(codePage) > -1)
        {
            tagFinder = Utf8;
            return true;
        }
        if (codePage == 29001)
        {
            tagFinder = Europa;
            return true;
        }
        if (MarkupEncodingInfos.EBCDIC.CodePages.IndexOf(codePage) > -1)
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