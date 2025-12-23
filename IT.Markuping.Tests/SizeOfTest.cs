using IT.Markuping.Encodings;
using IT.Markuping.Implementation;
using System.Runtime.CompilerServices;

namespace IT.Markuping.Tests;

internal class SizeOfTest
{
    [Test]
    public void Test()
    {
        Assert.That(Unsafe.SizeOf<TagEnding>(), Is.EqualTo(1));
        Assert.That(Unsafe.SizeOf<TagEndings>(), Is.EqualTo(1));

        Assert.That(Unsafe.SizeOf<Tag>(), Is.EqualTo(8));
        Assert.That(Unsafe.SizeOf<TagOpening>(), Is.EqualTo(8));
        Assert.That(Unsafe.SizeOf<TagClosing>(), Is.EqualTo(8));
        Assert.That(Unsafe.SizeOf<ByteEncoding.Tokens>(), Is.EqualTo(8));
        Assert.That(Unsafe.SizeOf<EquatableTagFinder<byte>.Tokens>(), Is.EqualTo(8));
        Assert.That(Unsafe.SizeOf<EquatableTagFinder<char>.Tokens>(), Is.EqualTo(16));
        Assert.That(Unsafe.SizeOf<EquatableTagFinder<int>.Tokens>(), Is.EqualTo(32));
        Assert.That(Unsafe.SizeOf<Tags>(), Is.EqualTo(16));
    }
}