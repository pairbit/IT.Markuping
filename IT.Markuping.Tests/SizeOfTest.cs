using IT.Markuping.Implementation;
using IT.Markuping.Internal;
using System.Runtime.CompilerServices;

namespace IT.Markuping.Tests;

internal class SizeOfTest
{
    [Test]
    public void Test()
    {
        Assert.That(Unsafe.SizeOf<TagEnding>(), Is.EqualTo(1));
        Assert.That(Unsafe.SizeOf<TagEndings>(), Is.EqualTo(1));

        Assert.That(Unsafe.SizeOf<StartEnd>(), Is.EqualTo(8));
        Assert.That(Unsafe.SizeOf<Tag>(), Is.EqualTo(8));
        Assert.That(Unsafe.SizeOf<TagNS>(), Is.EqualTo(8));
        Assert.That(Unsafe.SizeOf<TagOpening>(), Is.EqualTo(8));
        Assert.That(Unsafe.SizeOf<TagClosing>(), Is.EqualTo(8));
        Assert.That(Unsafe.SizeOf<MarkupFinder<byte>.Tokens>(), Is.EqualTo(8));
        Assert.That(Unsafe.SizeOf<MarkupFinder<char>.Tokens>(), Is.EqualTo(16));
        Assert.That(Unsafe.SizeOf<MarkupFinder<int>.Tokens>(), Is.EqualTo(32));
        Assert.That(Unsafe.SizeOf<Tags>(), Is.EqualTo(16));

        Assert.That(Unsafe.SizeOf<MarkupAlphabet<byte>>(), Is.EqualTo(16));
        Assert.That(Unsafe.SizeOf<MarkupAlphabet<int>>(), Is.EqualTo(16));

        Assert.That(Unsafe.SizeOf<MarkupTokens<byte>>(), Is.EqualTo(23));
        Assert.That(Unsafe.SizeOf<MarkupTokens<char>>(), Is.EqualTo(46));
        Assert.That(Unsafe.SizeOf<MarkupTokens<int>>(), Is.EqualTo(92));
    }
}