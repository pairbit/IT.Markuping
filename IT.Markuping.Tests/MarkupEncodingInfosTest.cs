using System;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace IT.Markuping.Tests;

internal class MarkupEncodingInfosTest
{
    [Test]
    public void CastTest()
    {
        CastTest(1200, MarkupEncodingByteInfos.Utf16, MarkupEncodingCharInfos.Utf16);
        CastTest(1200, MarkupEncodingByteInfos.Utf16_Strict, MarkupEncodingCharInfos.Utf16_Strict);
        CastTest(1201, MarkupEncodingByteInfos.Utf16BE, MarkupEncodingCharInfos.Utf16BE);
        CastTest(1201, MarkupEncodingByteInfos.Utf16BE_Strict, MarkupEncodingCharInfos.Utf16BE_Strict);
        CastTest(12000, MarkupEncodingByteInfos.Utf32, MarkupEncodingIntInfos.Utf32);
        CastTest(12000, MarkupEncodingByteInfos.Utf32_Strict, MarkupEncodingIntInfos.Utf32_Strict);
        CastTest(12001, MarkupEncodingByteInfos.Utf32BE, MarkupEncodingIntInfos.Utf32BE);
        CastTest(12001, MarkupEncodingByteInfos.Utf32BE_Strict, MarkupEncodingIntInfos.Utf32BE_Strict);
    }

    [Test]
    public void ByteTest()
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
                ByteTest(markupEncodingInfo, encoding);
            }
            else
            {
                Assert.Fail($"CodePage {codePage,5} not supported");
            }

            if (MarkupEncodingByteInfos.TryGetStrict(codePage, out markupEncodingInfo))
            {
                ByteTest(markupEncodingInfo, encoding);
            }
            else
            {
                Assert.Fail($"CodePage {codePage,5} not supported");
            }
        }
    }

    private static void ByteTest(MarkupAlphabetInfo<byte> markupEncodingInfo, Encoding encoding)
    {
        if (!encoding.IsSingleByte)
        {
            Console.WriteLine($"{encoding.CodePage} is not single byte");
        }

        Assert.That(markupEncodingInfo.CodePages.Contains(encoding.CodePage), Is.True);

        var markupEncoding = markupEncodingInfo.Alphabet;
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

    private static void CastTest<T>(int codePage, MarkupAlphabetInfo<byte> mebi, MarkupAlphabetInfo<T> meci) where T : unmanaged
    {
        Assert.That(mebi.CodePages.Contains(codePage), Is.True);
        Assert.That(meci.CodePages.Contains(codePage), Is.True);

        var meb = mebi.Alphabet;
        var mec = meci.Alphabet;

        var lt = Cast<T>(meb.LT);
        var gt = Cast<T>(meb.GT);
        var slash = Cast<T>(meb.Slash);
        var colon = Cast<T>(meb.Colon);
        var space = Cast<T>(meb.Space);
        var quot = Cast<T>(meb.Quot);
        var eq = Cast<T>(meb.Eq);
        var apos = Cast<T>(meb.Apos);
        var cr = Cast<T>(meb.CR);
        var lf = Cast<T>(meb.LF);
        var tab = Cast<T>(meb.Tab);

        Assert.That(mec.LT.SequenceEqual(lt), Is.True);
        Assert.That(mec.GT.SequenceEqual(gt), Is.True);
        Assert.That(mec.Slash.SequenceEqual(slash), Is.True);
        Assert.That(mec.Colon.SequenceEqual(colon), Is.True);
        Assert.That(mec.Space.SequenceEqual(space), Is.True);
        Assert.That(mec.Quot.SequenceEqual(quot), Is.True);
        Assert.That(mec.Eq.SequenceEqual(eq), Is.True);

        if (meb.IsStrict)
        {
            Assert.That(mec.IsStrict, Is.True);
            Assert.That(mec.Apos.IsEmpty, Is.EqualTo(meb.Apos.IsEmpty));
            Assert.That(mec.CR.IsEmpty, Is.EqualTo(meb.CR.IsEmpty));
            Assert.That(mec.LF.IsEmpty, Is.EqualTo(meb.LF.IsEmpty));
            Assert.That(mec.Tab.IsEmpty, Is.EqualTo(meb.Tab.IsEmpty));
        }
        else
        {
            Assert.That(mec.Apos.SequenceEqual(apos), Is.True);
            Assert.That(mec.CR.SequenceEqual(cr), Is.True);
            Assert.That(mec.LF.SequenceEqual(lf), Is.True);
            Assert.That(mec.Tab.SequenceEqual(tab), Is.True);
        }

        Assert.That(meb.IsComplex, Is.True);

        if (mec.IsComplex)
        {
            Console.WriteLine($"{codePage} is complex");
        }
        else
        {
            var tokens = (MarkupTokens<T>)mec;

            Assert.That(tokens.LT, Is.EqualTo(lt[0]));
            Assert.That(tokens.GT, Is.EqualTo(gt[0]));
            Assert.That(tokens.Slash, Is.EqualTo(slash[0]));
            Assert.That(tokens.Colon, Is.EqualTo(colon[0]));
            Assert.That(tokens.Space, Is.EqualTo(space[0]));
            Assert.That(tokens.Quot, Is.EqualTo(quot[0]));
            Assert.That(tokens.Eq, Is.EqualTo(eq[0]));

            if (mec.IsStrict)
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

    private static ReadOnlySpan<T> Cast<T>(ReadOnlySpan<byte> span) where T : unmanaged
       => MemoryMarshal.Cast<byte, T>(span);
}