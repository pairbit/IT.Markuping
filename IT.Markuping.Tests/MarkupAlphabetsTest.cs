using System;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace IT.Markuping.Tests;

internal class MarkupAlphabetsTest
{
    [Test]
    public void CastTest()
    {
        CastTest(MarkupAlphabets.Byte.Utf16, MarkupAlphabets.Char.Utf16);
        CastTest(MarkupAlphabets.Byte.Utf16BE, MarkupAlphabets.Char.Utf16BE);
        CastTest(MarkupAlphabets.Byte.Utf32, MarkupAlphabets.Int32.Utf32);
        CastTest(MarkupAlphabets.Byte.Utf32BE, MarkupAlphabets.Int32.Utf32BE);
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

            if (MarkupAlphabets.Byte.TryGet(codePage, out var alphabet))
            {
                ByteTest(alphabet, encoding);
            }
            else
            {
#if NET
                Assert.Fail($"CodePage {codePage,5} not supported");
#else
                Console.WriteLine($"CodePage {codePage,5} not supported");
#endif
            }
        }
    }

    private static void ByteTest(MarkupAlphabet<byte> alphabet, Encoding encoding)
    {
        var size = alphabet.Size;

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

        var excl = encoding.GetBytes("!");
        var dash = encoding.GetBytes("-");
        var lsqb = encoding.GetBytes("[");
        var rsqb = encoding.GetBytes("]");
        var quest = encoding.GetBytes("?");
        var x = encoding.GetBytes("x");
        var m = encoding.GetBytes("m");
        var l = encoding.GetBytes("l");
        var n = encoding.GetBytes("n");
        var s = encoding.GetBytes("s");
        var i = encoding.GetBytes("i");
        var d = encoding.GetBytes("d");

        Assert.That(alphabet.LT.SequenceEqual(lt), Is.True);
        Assert.That(alphabet.GT.SequenceEqual(gt), Is.True);
        Assert.That(alphabet.Slash.SequenceEqual(slash), Is.True);
        Assert.That(alphabet.Colon.SequenceEqual(colon), Is.True);
        Assert.That(alphabet.Space.SequenceEqual(space), Is.True);
        Assert.That(alphabet.Quot.SequenceEqual(quot), Is.True);
        Assert.That(alphabet.Eq.SequenceEqual(eq), Is.True);

        Assert.That(alphabet.Excl.SequenceEqual(excl), Is.True);
        Assert.That(alphabet.Dash.SequenceEqual(dash), Is.True);
        Assert.That(alphabet.LSQB.SequenceEqual(lsqb), Is.True);
        Assert.That(alphabet.RSQB.SequenceEqual(rsqb), Is.True);
        Assert.That(alphabet.Quest.SequenceEqual(quest), Is.True);
        Assert.That(alphabet.X.SequenceEqual(x), Is.True);
        Assert.That(alphabet.M.SequenceEqual(m), Is.True);
        Assert.That(alphabet.L.SequenceEqual(l), Is.True);
        Assert.That(alphabet.N.SequenceEqual(n), Is.True);
        Assert.That(alphabet.S.SequenceEqual(s), Is.True);
        Assert.That(alphabet.I.SequenceEqual(i), Is.True);
        Assert.That(alphabet.D.SequenceEqual(d), Is.True);

        if (alphabet.IsStrict)
        {
            Assert.That(alphabet.Apos.IsEmpty, Is.True);
            Assert.That(alphabet.CR.IsEmpty, Is.True);
            Assert.That(alphabet.LF.IsEmpty, Is.True);
            Assert.That(alphabet.Tab.IsEmpty, Is.True);

            Assert.That(alphabet.Abc.Length, Is.EqualTo(size * MarkupAlphabet.StrictLength));

            var abc = encoding.GetBytes("<>/: \"=");
            Assert.That(alphabet.Abc.SequenceEqual(abc), Is.True);
        }
        else
        {
            Assert.That(alphabet.Apos.SequenceEqual(apos), Is.True);
            Assert.That(alphabet.CR.SequenceEqual(cr), Is.True);
            Assert.That(alphabet.LF.SequenceEqual(lf), Is.True);
            Assert.That(alphabet.Tab.SequenceEqual(tab), Is.True);

            Assert.That(alphabet.Abc.Length, Is.EqualTo(size * MarkupAlphabet.Length));

            //<>/: \"='\r\n\t
            //<>/: \"=!-[]?xmlnsid'\r\n\t
            var abc = encoding.GetBytes("<>/: \"=!-[]?xmlnsid'\r\n\t");
            Assert.That(alphabet.Abc.SequenceEqual(abc), Is.True);
        }

        if (alphabet.IsComplex)
        {
            Console.WriteLine($"{encoding.CodePage} is complex");
        }
        else
        {
            var tokens = (MarkupTokens<byte>)alphabet;

            Assert.That(tokens.LT, Is.EqualTo(lt[0]));
            Assert.That(tokens.GT, Is.EqualTo(gt[0]));
            Assert.That(tokens.Slash, Is.EqualTo(slash[0]));
            Assert.That(tokens.Colon, Is.EqualTo(colon[0]));
            Assert.That(tokens.Space, Is.EqualTo(space[0]));
            Assert.That(tokens.Quot, Is.EqualTo(quot[0]));
            Assert.That(tokens.Eq, Is.EqualTo(eq[0]));

            Assert.That(tokens.Excl, Is.EqualTo(excl[0]));
            Assert.That(tokens.Dash, Is.EqualTo(dash[0]));
            Assert.That(tokens.LSQB, Is.EqualTo(lsqb[0]));
            Assert.That(tokens.RSQB, Is.EqualTo(rsqb[0]));
            Assert.That(tokens.Quest, Is.EqualTo(quest[0]));
            Assert.That(tokens.X, Is.EqualTo(x[0]));
            Assert.That(tokens.M, Is.EqualTo(m[0]));
            Assert.That(tokens.L, Is.EqualTo(l[0]));
            Assert.That(tokens.N, Is.EqualTo(n[0]));
            Assert.That(tokens.S, Is.EqualTo(s[0]));
            Assert.That(tokens.I, Is.EqualTo(i[0]));
            Assert.That(tokens.D, Is.EqualTo(d[0]));

            if (alphabet.IsStrict)
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

    private static void CastTest<T>(MarkupAlphabet<byte> abcByte, MarkupAlphabet<T> abc) where T : unmanaged, IEquatable<T>
    {
        var lt = Cast<T>(abcByte.LT);
        var gt = Cast<T>(abcByte.GT);
        var slash = Cast<T>(abcByte.Slash);
        var colon = Cast<T>(abcByte.Colon);
        var space = Cast<T>(abcByte.Space);
        var quot = Cast<T>(abcByte.Quot);
        var eq = Cast<T>(abcByte.Eq);
        var apos = Cast<T>(abcByte.Apos);
        var cr = Cast<T>(abcByte.CR);
        var lf = Cast<T>(abcByte.LF);
        var tab = Cast<T>(abcByte.Tab);

        var excl = Cast<T>(abcByte.Excl);
        var dash = Cast<T>(abcByte.Dash);
        var lsqb = Cast<T>(abcByte.LSQB);
        var rsqb = Cast<T>(abcByte.RSQB);
        var quest = Cast<T>(abcByte.Quest);
        var x = Cast<T>(abcByte.X);
        var m = Cast<T>(abcByte.M);
        var l = Cast<T>(abcByte.L);
        var n = Cast<T>(abcByte.N);
        var s = Cast<T>(abcByte.S);
        var i = Cast<T>(abcByte.I);
        var d = Cast<T>(abcByte.D);

        Assert.That(abc.LT.SequenceEqual(lt), Is.True);
        Assert.That(abc.GT.SequenceEqual(gt), Is.True);
        Assert.That(abc.Slash.SequenceEqual(slash), Is.True);
        Assert.That(abc.Colon.SequenceEqual(colon), Is.True);
        Assert.That(abc.Space.SequenceEqual(space), Is.True);
        Assert.That(abc.Quot.SequenceEqual(quot), Is.True);
        Assert.That(abc.Eq.SequenceEqual(eq), Is.True);

        Assert.That(abc.Excl.SequenceEqual(excl), Is.True);
        Assert.That(abc.Dash.SequenceEqual(dash), Is.True);
        Assert.That(abc.LSQB.SequenceEqual(lsqb), Is.True);
        Assert.That(abc.RSQB.SequenceEqual(rsqb), Is.True);
        Assert.That(abc.Quest.SequenceEqual(quest), Is.True);
        Assert.That(abc.X.SequenceEqual(x), Is.True);
        Assert.That(abc.M.SequenceEqual(m), Is.True);
        Assert.That(abc.L.SequenceEqual(l), Is.True);
        Assert.That(abc.N.SequenceEqual(n), Is.True);
        Assert.That(abc.S.SequenceEqual(s), Is.True);
        Assert.That(abc.I.SequenceEqual(i), Is.True);
        Assert.That(abc.D.SequenceEqual(d), Is.True);

        if (abc.IsStrict)
        {
            Assert.That(abcByte.IsStrict, Is.True);
            Assert.That(abc.Apos.IsEmpty, Is.EqualTo(abcByte.Apos.IsEmpty));
            Assert.That(abc.CR.IsEmpty, Is.EqualTo(abcByte.CR.IsEmpty));
            Assert.That(abc.LF.IsEmpty, Is.EqualTo(abcByte.LF.IsEmpty));
            Assert.That(abc.Tab.IsEmpty, Is.EqualTo(abcByte.Tab.IsEmpty));
        }
        else
        {
            Assert.That(abc.Apos.SequenceEqual(apos), Is.True);
            Assert.That(abc.CR.SequenceEqual(cr), Is.True);
            Assert.That(abc.LF.SequenceEqual(lf), Is.True);
            Assert.That(abc.Tab.SequenceEqual(tab), Is.True);
        }

        Assert.That(abcByte.IsComplex, Is.True);
        Assert.That(abc.IsComplex, Is.False);

        var tokens = (MarkupTokens<T>)abc;

        Assert.That(tokens.LT, Is.EqualTo(lt[0]));
        Assert.That(tokens.GT, Is.EqualTo(gt[0]));
        Assert.That(tokens.Slash, Is.EqualTo(slash[0]));
        Assert.That(tokens.Colon, Is.EqualTo(colon[0]));
        Assert.That(tokens.Space, Is.EqualTo(space[0]));
        Assert.That(tokens.Quot, Is.EqualTo(quot[0]));
        Assert.That(tokens.Eq, Is.EqualTo(eq[0]));

        Assert.That(tokens.Excl, Is.EqualTo(excl[0]));
        Assert.That(tokens.Dash, Is.EqualTo(dash[0]));
        Assert.That(tokens.LSQB, Is.EqualTo(lsqb[0]));
        Assert.That(tokens.RSQB, Is.EqualTo(rsqb[0]));
        Assert.That(tokens.Quest, Is.EqualTo(quest[0]));
        Assert.That(tokens.X, Is.EqualTo(x[0]));
        Assert.That(tokens.M, Is.EqualTo(m[0]));
        Assert.That(tokens.L, Is.EqualTo(l[0]));
        Assert.That(tokens.N, Is.EqualTo(n[0]));
        Assert.That(tokens.S, Is.EqualTo(s[0]));
        Assert.That(tokens.I, Is.EqualTo(i[0]));
        Assert.That(tokens.D, Is.EqualTo(d[0]));

        if (abc.IsStrict)
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

    private static ReadOnlySpan<T> Cast<T>(ReadOnlySpan<byte> span) where T : unmanaged
       => MemoryMarshal.Cast<byte, T>(span);
}