using System;
using System.Linq;
using System.Text;

namespace IT.Markuping.Tests;

internal class MarkupEncodingInfosTest
{
    [Test]
    public void Test()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        var encodingInfos = Encoding.GetEncodings();

        Console.WriteLine($"encodingInfos count: {encodingInfos.Length}");

        foreach (var encodingInfo in encodingInfos)
        {
            var codePage = encodingInfo.CodePage;

            if (MarkupEncodingInfos.TryGet(codePage, out var markupEncodingInfo))
            {
                Test(markupEncodingInfo, encodingInfo.GetEncoding());
            }
            else
            {
                Assert.Fail($"CodePage {codePage,5} not supported");
            }
        }
    }

    private void Test(MarkupEncodingInfo markupEncodingInfo, Encoding encoding)
    {
        if (!encoding.IsSingleByte)
        {
            Console.WriteLine($"{encoding.CodePage} is not single byte");
        }

        Assert.That(markupEncodingInfo.CodePages.Contains(encoding.CodePage), Is.True);

        var markupEncoding = markupEncodingInfo.Encoding;
        
        var lt = encoding.GetBytes("<");
        var gt = encoding.GetBytes(">");
        var slash = encoding.GetBytes("/");
        var colon = encoding.GetBytes(":");
        var space = encoding.GetBytes(" ");
        var quot = encoding.GetBytes("\"");
        var eq = encoding.GetBytes("=");
        var apos = encoding.GetBytes("'");
        var cr = encoding.GetBytes("\r");
        var lf = encoding.GetBytes("\n");
        var tab = encoding.GetBytes("\t");

        Assert.That(markupEncoding.LT.SequenceEqual(lt), Is.True);
        Assert.That(markupEncoding.GT.SequenceEqual(gt), Is.True);
        Assert.That(markupEncoding.Slash.SequenceEqual(slash), Is.True);
        Assert.That(markupEncoding.Colon.SequenceEqual(colon), Is.True);
        Assert.That(markupEncoding.Space.SequenceEqual(space), Is.True);
        Assert.That(markupEncoding.Quot.SequenceEqual(quot), Is.True);
        Assert.That(markupEncoding.Eq.SequenceEqual(eq), Is.True);

        if (markupEncoding.IsStrict)
        {
            Assert.That(markupEncoding.Apos.IsEmpty, Is.True);
            Assert.That(markupEncoding.CR.IsEmpty, Is.True);
            Assert.That(markupEncoding.LF.IsEmpty, Is.True);
            Assert.That(markupEncoding.Tab.IsEmpty, Is.True);

            var abc = encoding.GetBytes("<>/: \"=");
            Assert.That(markupEncoding.Abc.SequenceEqual(abc), Is.True);
        }
        else
        {
            Assert.That(markupEncoding.Apos.SequenceEqual(apos), Is.True);
            Assert.That(markupEncoding.CR.SequenceEqual(cr), Is.True);
            Assert.That(markupEncoding.LF.SequenceEqual(lf), Is.True);
            Assert.That(markupEncoding.Tab.SequenceEqual(tab), Is.True);

            var abc = encoding.GetBytes("<>/: \"='\r\n\t");
            Assert.That(markupEncoding.Abc.SequenceEqual(abc), Is.True);
        }
    }
}