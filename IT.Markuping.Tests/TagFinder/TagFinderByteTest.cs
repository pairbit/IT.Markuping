using IT.Markuping.Implementation;
using System;
using System.Text;

namespace IT.Markuping.Tests;

internal class TagFinderByteTest
{
    [Test]
    public void Tester()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        var encodingInfos = Encoding.GetEncodings();

        Console.WriteLine($"encodingInfos count: {encodingInfos.Length}");

        foreach (var encodingInfo in encodingInfos)
        {
            var codePage = encodingInfo.CodePage;

            if (TagFinderByte.TryGet(codePage, out var tagFinder))
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