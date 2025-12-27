using IT.Markuping.Interfaces;
using IT.Markuping.Internal;
using System;
using System.Text;

namespace IT.Markuping.Tests;

internal class TagFinderByteTest
{
    //[Test]
    public void Tester_Utf()
    {
        Test(TagFinders.Utf8, Encoding.UTF8);
        Test(TagFinders.Utf16, Encoding.Unicode);
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

            if (TagFinders.TryGet(codePage, out var tagFinder))
            {
                Test(tagFinder, encoding);
            }
            else
            {
#if NET
                Assert.Fail($"CodePage {codePage,5} not supported");
#else
                Console.WriteLine($"CodePage {codePage,5} not supported");
#endif
            }

            if (MarkupCodePages.Utf8.AsSpan().IndexOf(codePage) > -1)
            {
                Test(TagFinders.OtherSpaces_Utf8, encoding);
            }
            else if (codePage == 29001)
            {
                Test(TagFinders.OtherSpace_Europa, encoding);
            }
            else if (MarkupCodePages.EBCDIC.AsSpan().IndexOf(codePage) > -1)
            {
                Test(TagFinders.OtherSpaces_EBCDIC, encoding);
            }
            else if (codePage == 1026 || codePage == 20905)
            {
                Test(TagFinders.OtherSpaces_EBCDIC_Turkish, encoding);
            }
            else if (codePage == 1047 || codePage == 20924)
            {
                Test(TagFinders.OtherSpaces_IBM_Latin1, encoding);
            }
            else if (codePage == 1200)
            {
                Test(TagFinders.Complex_Utf16, encoding);
            }
            else if (codePage == 1201)
            {
                Test(TagFinders.Complex_Utf16BE, encoding);
            }
            else if (codePage == 12000)
            {
                Test(TagFinders.Complex_Utf32, encoding);
            }
            else if (codePage == 12001)
            {
                Test(TagFinders.Complex_Utf32BE, encoding);
            }
        }
    }

    private static void Test(ITagFinder<byte> finder, Encoding encoding)
    {
        var tester = new TagFinderByteTester(finder, encoding);
        tester.Test();
    }
}