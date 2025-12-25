using System;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace IT.Markuping.Tests;

public class MarkupEncodingCharInfosTest
{
    [Test]
    public void Utf16_Test()
    {
        Test(Encoding.Unicode, MarkupEncodingByteInfos.Utf16, MarkupEncodingCharInfos.Utf16);
        Test(Encoding.Unicode, MarkupEncodingByteInfos.Utf16_Strict, MarkupEncodingCharInfos.Utf16_Strict);
        //Test(Encoding.BigEndianUnicode, MarkupEncodingByteInfos.Utf16BE, MarkupEncodingCharInfos.Utf16);
    }

    private static void Test(Encoding encoding, MarkupEncodingInfo<byte> mebi, MarkupEncodingInfo<char> meci)
    {
        Assert.That(mebi.CodePages.Contains(encoding.CodePage), Is.True);
        Assert.That(meci.CodePages.Contains(encoding.CodePage), Is.True);

        var meb = mebi.Encoding;
        var mec = meci.Encoding;

        var lt = Cast(meb.LT);
        var gt = Cast(meb.GT);
        var slash = Cast(meb.Slash);
        var colon = Cast(meb.Colon);
        var space = Cast(meb.Space);
        var quot = Cast(meb.Quot);
        var eq = Cast(meb.Eq);
        var apos = Cast(meb.Apos);
        var cr = Cast(meb.CR);
        var lf = Cast(meb.LF);
        var tab = Cast(meb.Tab);

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
            Console.WriteLine($"{encoding.CodePage} is complex");
        }
        else
        {
            var tokens = (MarkupTokens<char>)mec;

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

    private static ReadOnlySpan<char> Cast(ReadOnlySpan<byte> span)
        => MemoryMarshal.Cast<byte, char>(span);
}