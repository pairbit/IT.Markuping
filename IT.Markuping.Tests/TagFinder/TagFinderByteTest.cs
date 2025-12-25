using IT.Markuping.Implementation;
using IT.Markuping.Interfaces;
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
                Assert.Fail($"{codePage,5} Options not supported");
            }

            if (MarkupEncodingInfos.Utf8.CodePages.Contains(codePage))
            {
                Test(TagFinderByte.Utf8, encoding);
            }
            else if (MarkupEncodingInfos.Europa.CodePages.Contains(codePage))
            {
                Test(TagFinderByte.Europa, encoding);
            }
            else if (MarkupEncodingInfos.EBCDIC.CodePages.Contains(codePage))
            {
                Test(TagFinderByte.EBCDIC, encoding);
            }
            else if (MarkupEncodingInfos.EBCDIC_Turkish.CodePages.Contains(codePage))
            {
                Test(TagFinderByte.EBCDIC_Turkish, encoding);
            }
            else if (MarkupEncodingInfos.IBM_Latin1.CodePages.Contains(codePage))
            {
                Test(TagFinderByte.IBM_Latin1, encoding);
            }
        }
    }

    private static void Test(ITagFinder<byte> finder, Encoding encoding)
    {
        var tester = new TagFinderByteTester(finder, encoding);
        tester.Test();
    }
}