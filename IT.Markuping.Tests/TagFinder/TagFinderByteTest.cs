using IT.Markuping.Implementation;
using IT.Markuping.Interfaces;
using System;
using System.Text;
using IT.Markuping.Extensions;

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

            if (MarkupCodePages.Utf8.AsSpan().Contains(codePage))
            {
                Test(TagFinderByte.Utf8, encoding);
            }
            else if (codePage == 29001)
            {
                Test(TagFinderByte.Europa, encoding);
            }
            else if (MarkupCodePages.EBCDIC.AsSpan().Contains(codePage))
            {
                Test(TagFinderByte.EBCDIC, encoding);
            }
            else if (codePage == 1026 || codePage == 20905)
            {
                Test(TagFinderByte.EBCDIC_Turkish, encoding);
            }
            else if (codePage == 1047 || codePage == 20924)
            {
                Test(TagFinderByte.IBM_Latin1, encoding);
            }
            //else if (codePage == 1200)
            //{
            //    Test(TagFinderChar.Utf16.AsProxy(), encoding);
            //}
            //else if (codePage == 1201)
            //{
            //    Test(TagFinderChar.Utf16BE.AsProxy(), encoding);
            //}
            //else if (codePage == 12000)
            //{
            //    Test(TagFinderInt32.Utf32.AsProxy(), encoding);
            //}
            //else if (codePage == 12001)
            //{
            //    Test(TagFinderInt32.Utf32BE.AsProxy(), encoding);
            //}
        }
    }

    private static void Test(ITagFinder<byte> finder, Encoding encoding)
    {
        var tester = new TagFinderByteTester(finder, encoding);
        tester.Test();
    }
}