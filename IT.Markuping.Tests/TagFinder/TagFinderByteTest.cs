using IT.Markuping.Implementation;
using System;
using System.Text;

namespace IT.Markuping.Tests;

internal class TagFinderByteTest
{
    //[Test]
    public void Tester_Utf()
    {
        var tester = new TagFinderByteTester(TagFinders.Utf8, Encoding.UTF8);

        tester.Test();

        tester = new TagFinderByteTester(TagFinders.Utf16, Encoding.Unicode);

        tester.Test();
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

            if (TagFinders.TryGet(codePage, out var tagFinder))
            {
                var encoding = encodingInfo.GetEncoding();
                var tester = new TagFinderByteTester(tagFinder, encoding);

                tester.Test();
            }
            else
            {
                Assert.Fail($"{codePage,5} Options not supported");
            }
        }
    }
}