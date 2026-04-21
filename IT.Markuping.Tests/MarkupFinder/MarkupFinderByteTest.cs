using IT.Markuping.Interfaces;
using System;
using System.Text;

namespace IT.Markuping.Tests;

internal class MarkupFinderByteTest
{
    private static readonly INameEquatable _nameUtf8 = new IdEquatabe<byte>(MarkupAlphabets.Byte.Utf8);
    private static readonly INameEquatable _nameUtf16 = new IdEquatabe<char>(MarkupAlphabets.Char.Utf16);
    private static readonly INameEquatable _nameUtf16BE = new IdEquatabe<short>(MarkupAlphabets.Int16.Utf16BE);
    private static readonly INameEquatable _nameUtf32 = new IdEquatabe<uint>(MarkupAlphabets.UInt32.Utf32);
    private static readonly INameEquatable _nameUtf32BE = new IdEquatabe<int>(MarkupAlphabets.Int32.Utf32BE);
    private static readonly INameEquatable _nameEBCDIC = new IdEquatabe<byte>(MarkupAlphabets.Byte.EBCDIC);

    [Test]
    public void Tester_Utf8()
    {
        Test(MarkupFinders.Utf8, Encoding.UTF8, _nameUtf8);
    }

    [Test]
    public void Tester_EBCDIC()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
        Test(MarkupFinders.EBCDIC, Encoding.GetEncoding(500), _nameEBCDIC);
    }

    [Test]
    public void Tester_Utf16()
    {
        Test(MarkupFinders.Utf16, Encoding.Unicode, _nameUtf16);
    }

    [Test]
    public void Tester_Utf16BE()
    {
        Test(MarkupFinders.Utf16BE, Encoding.BigEndianUnicode, _nameUtf16BE);
    }

    [Test]
    public void Tester_Utf32()
    {
        Test(MarkupFinders.Utf32, Encoding.UTF32, _nameUtf32);
    }

    [Test]
    public void Tester_Utf32BE()
    {
        Test(MarkupFinders.Utf32BE, Encoding.GetEncoding(12001), _nameUtf32BE);
    }

    [Test]
    public void Tester()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        var encodingInfos = Encoding.GetEncodings();

        Console.WriteLine($"encodingInfos count: {encodingInfos.Length}");

        foreach (var encodingInfo in encodingInfos)
        {
            var codePage = encodingInfo.CodePage;
            var encoding = encodingInfo.GetEncoding();
            
            if (MarkupFinders.TryGet(codePage, out var finder))
            {
                var name = GetName(codePage);
                try
                {
                    Test(finder, encoding, name);
                }
                catch
                {
                    Console.WriteLine($"CodePage {codePage,5} error");
                    throw;
                }

                if (MarkupFinders.CodePages.Utf8.AsSpan().IndexOf(codePage) > -1)
                {
                    Test(MarkupFinders.OtherSpaces.Utf8, encoding, name);
                }
                else if (codePage == 29001)
                {
                    Test(MarkupFinders.OtherSpaces.Europa, encoding, name);
                }
                else if (MarkupFinders.CodePages.EBCDIC.AsSpan().IndexOf(codePage) > -1)
                {
                    Test(MarkupFinders.OtherSpaces.EBCDIC, encoding, name);
                }
                else if (codePage == 1026 || codePage == 20905)
                {
                    Test(MarkupFinders.OtherSpaces.EBCDIC_Turkish, encoding, name);
                }
                else if (codePage == 1047 || codePage == 20924)
                {
                    Test(MarkupFinders.OtherSpaces.EBCDIC_IBM_Latin1, encoding, name);
                }
                else if (codePage == 1200)
                {
                    Test(MarkupFinders.Complex.Utf16, encoding, null!);
                }
                else if (codePage == 1201)
                {
                    Test(MarkupFinders.Complex.Utf16BE, encoding, null!);
                }
                else if (codePage == 12000)
                {
                    Test(MarkupFinders.Complex.Utf32, encoding, null!);
                }
                else if (codePage == 12001)
                {
                    Test(MarkupFinders.Complex.Utf32BE, encoding, null!);
                }
            }
            else
            {
#if NET
                Assert.Fail($"CodePage {codePage,5} not supported");
#else
                Console.WriteLine($"CodePage {codePage,5} not supported");
#endif
            }
        }
    }

    private static void Test(IMarkupFinder<byte> finder, Encoding encoding, INameEquatable name)
    {
        Assert.That(finder.CodePages.IndexOf(encoding.CodePage) > -1);

        var tester = new MarkupFinderByteTester(finder, encoding, name);
        tester.Test();
    }

    private static INameEquatable GetName(int codePage)
    {
        if (codePage == 1200) return _nameUtf16;

        if (codePage == 1201) return _nameUtf16BE;

        if (codePage == 12000) return _nameUtf32;

        if (codePage == 12001) return _nameUtf32BE;

        if (MarkupAlphabets.Byte.TryGet(codePage, out var alphabet))
        {
            return new IdEquatabe<byte>(alphabet);
        }

        throw new NotImplementedException($"CodePage {codePage,5}");
    }
}