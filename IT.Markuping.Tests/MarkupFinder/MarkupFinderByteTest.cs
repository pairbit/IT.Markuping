using IT.Markuping.Interfaces;
using IT.Markuping.Internal;
using System;
using System.Text;

namespace IT.Markuping.Tests;

internal class MarkupFinderByteTest
{
    //[Test]
    public void Tester_Utf()
    {
        Test(MarkupFinders.Utf8, Encoding.UTF8);
        Test(MarkupFinders.Utf16, Encoding.Unicode);
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
                try
                {
                    Test(finder, encoding);
                }
                catch
                {
                    Console.WriteLine($"CodePage {codePage,5} error");
                    throw;
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

            if (MarkupFinders.CodePages.Utf8.AsSpan().IndexOf(codePage) > -1)
            {
                Test(MarkupFinders.OtherSpaces.Utf8, encoding);
            }
            else if (codePage == 29001)
            {
                Test(MarkupFinders.OtherSpaces.Europa, encoding);
            }
            else if (MarkupFinders.CodePages.EBCDIC.AsSpan().IndexOf(codePage) > -1)
            {
                Test(MarkupFinders.OtherSpaces.EBCDIC, encoding);
            }
            else if (codePage == 1026 || codePage == 20905)
            {
                Test(MarkupFinders.OtherSpaces.EBCDIC_Turkish, encoding);
            }
            else if (codePage == 1047 || codePage == 20924)
            {
                Test(MarkupFinders.OtherSpaces.EBCDIC_IBM_Latin1, encoding);
            }
            else if (codePage == 1200)
            {
                Test(MarkupFinders.Complex.Utf16, encoding);
            }
            else if (codePage == 1201)
            {
                Test(MarkupFinders.Complex.Utf16BE, encoding);
            }
            else if (codePage == 12000)
            {
                Test(MarkupFinders.Complex.Utf32, encoding);
            }
            else if (codePage == 12001)
            {
                Test(MarkupFinders.Complex.Utf32BE, encoding);
            }
        }
    }

    private static void Test(IMarkupFinder<byte> finder, Encoding encoding)
    {
        var tester = new MarkupFinderByteTester(finder, encoding);
        tester.Test();
    }
}