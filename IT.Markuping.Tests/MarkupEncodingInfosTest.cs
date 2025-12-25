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
            var encoding = encodingInfo.GetEncoding();

            if (MarkupEncodingByteInfos.TryGet(codePage, out var markupEncodingInfo))
            {
                Test(markupEncodingInfo, encoding);
            }
            else
            {
                Assert.Fail($"CodePage {codePage,5} not supported");
            }

            if (MarkupEncodingByteInfos.TryGetStrict(codePage, out markupEncodingInfo))
            {
                Test(markupEncodingInfo, encoding);
            }
            else
            {
                Assert.Fail($"CodePage {codePage,5} not supported");
            }
        }
    }

    private void Test(MarkupEncodingInfo<byte> markupEncodingInfo, Encoding encoding)
    {
        if (!encoding.IsSingleByte)
        {
            Console.WriteLine($"{encoding.CodePage} is not single byte");
        }

        Assert.That(markupEncodingInfo.CodePages.Contains(encoding.CodePage), Is.True);

        var markupEncoding = markupEncodingInfo.Encoding;
        var size = markupEncoding.Size;

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

            Assert.That(markupEncoding.Abc.Length, Is.EqualTo(size * 7));

            var abc = encoding.GetBytes("<>/: \"=");
            Assert.That(markupEncoding.Abc.SequenceEqual(abc), Is.True);
        }
        else
        {
            Assert.That(markupEncoding.Apos.SequenceEqual(apos), Is.True);
            Assert.That(markupEncoding.CR.SequenceEqual(cr), Is.True);
            Assert.That(markupEncoding.LF.SequenceEqual(lf), Is.True);
            Assert.That(markupEncoding.Tab.SequenceEqual(tab), Is.True);

            Assert.That(markupEncoding.Abc.Length, Is.EqualTo(size * 11));

            var abc = encoding.GetBytes("<>/: \"='\r\n\t");
            Assert.That(markupEncoding.Abc.SequenceEqual(abc), Is.True);
        }

        if (markupEncoding.IsComplex)
        {
            Console.WriteLine($"{encoding.CodePage} is complex");
        }
        else
        {
            var tokens = (MarkupTokens<byte>)markupEncoding;

            Assert.That(tokens.LT, Is.EqualTo(lt[0]));
            Assert.That(tokens.GT, Is.EqualTo(gt[0]));
            Assert.That(tokens.Slash, Is.EqualTo(slash[0]));
            Assert.That(tokens.Colon, Is.EqualTo(colon[0]));
            Assert.That(tokens.Space, Is.EqualTo(space[0]));
            Assert.That(tokens.Quot, Is.EqualTo(quot[0]));
            Assert.That(tokens.Eq, Is.EqualTo(eq[0]));

            if (markupEncoding.IsStrict)
            {
                Assert.That(tokens.Apos, Is.Zero);
                Assert.That(tokens.CR, Is.Zero);
                Assert.That(tokens.LF, Is.Zero);
                Assert.That(tokens.Tab, Is.Zero);
            }
            else
            {
                Assert.That(tokens.Apos, Is.EqualTo(apos[0]));
                Assert.That(tokens.CR, Is.EqualTo(cr[0]));
                Assert.That(tokens.LF, Is.EqualTo(lf[0]));
                Assert.That(tokens.Tab, Is.EqualTo(tab[0]));
            }
        }
    }
}