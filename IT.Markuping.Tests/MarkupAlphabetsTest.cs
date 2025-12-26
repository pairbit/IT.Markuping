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
        CastTest(MarkupAlphabets.Byte.Utf16_Strict, MarkupAlphabets.Char.Utf16_Strict);
        CastTest(MarkupAlphabets.Byte.Utf16BE, MarkupAlphabets.Char.Utf16BE);
        CastTest(MarkupAlphabets.Byte.Utf16BE_Strict, MarkupAlphabets.Char.Utf16BE_Strict);
        CastTest(MarkupAlphabets.Byte.Utf32, MarkupAlphabets.Int32.Utf32);
        CastTest(MarkupAlphabets.Byte.Utf32_Strict, MarkupAlphabets.Int32.Utf32_Strict);
        CastTest(MarkupAlphabets.Byte.Utf32BE, MarkupAlphabets.Int32.Utf32BE);
        CastTest(MarkupAlphabets.Byte.Utf32BE_Strict, MarkupAlphabets.Int32.Utf32BE_Strict);
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

            if (MarkupAlphabets.Byte.TryGetStrict(codePage, out alphabet))
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

        Assert.That(alphabet.LT.SequenceEqual(lt), Is.True);
        Assert.That(alphabet.GT.SequenceEqual(gt), Is.True);
        Assert.That(alphabet.Slash.SequenceEqual(slash), Is.True);
        Assert.That(alphabet.Colon.SequenceEqual(colon), Is.True);
        Assert.That(alphabet.Space.SequenceEqual(space), Is.True);
        Assert.That(alphabet.Quot.SequenceEqual(quot), Is.True);
        Assert.That(alphabet.Eq.SequenceEqual(eq), Is.True);

        if (alphabet.IsStrict)
        {
            Assert.That(alphabet.Apos.IsEmpty, Is.True);
            Assert.That(alphabet.CR.IsEmpty, Is.True);
            Assert.That(alphabet.LF.IsEmpty, Is.True);
            Assert.That(alphabet.Tab.IsEmpty, Is.True);

            Assert.That(alphabet.Abc.Length, Is.EqualTo(size * 7));

            var abc = encoding.GetBytes("<>/: \"=");
            Assert.That(alphabet.Abc.SequenceEqual(abc), Is.True);
        }
        else
        {
            Assert.That(alphabet.Apos.SequenceEqual(apos), Is.True);
            Assert.That(alphabet.CR.SequenceEqual(cr), Is.True);
            Assert.That(alphabet.LF.SequenceEqual(lf), Is.True);
            Assert.That(alphabet.Tab.SequenceEqual(tab), Is.True);

            Assert.That(alphabet.Abc.Length, Is.EqualTo(size * 11));

            var abc = encoding.GetBytes("<>/: \"='\r\n\t");
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

        Assert.That(abc.LT.SequenceEqual(lt), Is.True);
        Assert.That(abc.GT.SequenceEqual(gt), Is.True);
        Assert.That(abc.Slash.SequenceEqual(slash), Is.True);
        Assert.That(abc.Colon.SequenceEqual(colon), Is.True);
        Assert.That(abc.Space.SequenceEqual(space), Is.True);
        Assert.That(abc.Quot.SequenceEqual(quot), Is.True);
        Assert.That(abc.Eq.SequenceEqual(eq), Is.True);

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