using System;

namespace IT.Markuping.Tests;

internal class TagNSTest
{
    [Test]
    public void CtorTest()
    {
        var tag = new TagNS();
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(0));
        Assert.That(tag.Length, Is.EqualTo(0));
        Assert.That(tag.ToString(), Is.EqualTo("0..0"));

        tag = default;
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(0));
        Assert.That(tag.Length, Is.EqualTo(0));

        tag = new TagNS(0, 1);
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(1));
        Assert.That(tag.Length, Is.EqualTo(1));
        Assert.That(tag.ToString(), Is.EqualTo("0..1"));

        tag = new TagNS(10, 100);
        Assert.That(tag.Start, Is.EqualTo(10));
        Assert.That(tag.End, Is.EqualTo(100));
        Assert.That(tag.Length, Is.EqualTo(90));
        Assert.That(tag.ToString(), Is.EqualTo("10..100"));

        tag = new TagNS(5, 10);
        Assert.That(tag.Start, Is.EqualTo(5));
        Assert.That(tag.End, Is.EqualTo(10));
        Assert.That(tag.Length, Is.EqualTo(5));
        Assert.That(tag.ToString(), Is.EqualTo("5..10"));

        tag = new TagNS(1, 3);
        Assert.That(tag.Start, Is.EqualTo(1));
        Assert.That(tag.End, Is.EqualTo(3));
        Assert.That(tag.ToString(), Is.EqualTo("1..3"));
    }

    [Test]
    public void AddOffsetTest()
    {
        var tag = new TagNS(0, 1);
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(1));

        tag = tag.AddOffset(10);
        Assert.That(tag.Start, Is.EqualTo(10));
        Assert.That(tag.End, Is.EqualTo(11));

        tag = tag.AddOffset(-10);
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(1));

        var ex1 = Assert.Throws<OverflowException>(() => tag.AddOffset(int.MaxValue));

        var ex2 = Assert.Throws<ArgumentOutOfRangeException>(() => tag.AddOffset(-1));
        Assert.That(ex2.ParamName, Is.EqualTo("offset"));

        var maxOffset = int.MaxValue - 1;

        tag = tag.AddOffset(maxOffset);
        Assert.That(tag.Start, Is.EqualTo(maxOffset));
        Assert.That(tag.End, Is.EqualTo(maxOffset + 1));

        ex2 = Assert.Throws<ArgumentOutOfRangeException>(() => tag.AddOffset(int.MinValue));
        Assert.That(ex2.ParamName, Is.EqualTo("offset"));

        tag = new TagNS(0, 1);
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(1));

        tag = tag.AddOffset(100);
        Assert.That(tag.Start, Is.EqualTo(100));
        Assert.That(tag.End, Is.EqualTo(101));

        tag = tag.AddOffset(-100);
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(1));

        ex1 = Assert.Throws<OverflowException>(() => tag.AddOffset(int.MaxValue));

        ex2 = Assert.Throws<ArgumentOutOfRangeException>(() => tag.AddOffset(-1));
        Assert.That(ex2.ParamName, Is.EqualTo("offset"));

        tag = tag.AddOffset(maxOffset);
        Assert.That(tag.Start, Is.EqualTo(maxOffset));
        Assert.That(tag.End, Is.EqualTo(maxOffset + 1));

        ex2 = Assert.Throws<ArgumentOutOfRangeException>(() => tag.AddOffset(int.MinValue));
        Assert.That(ex2.ParamName, Is.EqualTo("offset"));
    }

#if NET
    [Test]
    public void TryFormatTest()
    {
        var tag = new TagNS(10, 11);
        Assert.That(tag.TryFormat(stackalloc char[4], out var written), Is.False);
        Assert.That(written == 0, Is.True);

        Span<char> span = stackalloc char[5];
        Assert.That(tag.TryFormat(span, out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[6];
        Assert.That(tag.TryFormat(span, out written), Is.True);
        Assert.That(written == 6, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("10..11"));

        tag = new TagNS(12345, 123456);
        span = stackalloc char[12];
        Assert.That(tag.TryFormat(span, out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[13];
        Assert.That(tag.TryFormat(span, out written), Is.True);
        Assert.That(written == 13, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("12345..123456"));
    }
#endif

    [Test]
    public void CompareToTest()
    {
        var tag1 = new TagNS(100, 101);
        var tag2 = new TagNS(101, 102);

        Assert.That(tag1 < tag2, Is.True);
        Assert.That(tag2 > tag1, Is.True);

        tag2 = tag1;
        Assert.That(tag1 <= tag2, Is.True);
        Assert.That(tag1 >= tag2, Is.True);
    }

    [Test]
    public void InvalidTest()
    {
        var ex = Assert.Throws<ArgumentOutOfRangeException>(() => new TagNS(-1, 0));
        Assert.That(ex.ParamName, Is.EqualTo("start"));

        ex = Assert.Throws<ArgumentOutOfRangeException>(() => new TagNS(0, 0));
        Assert.That(ex.ParamName, Is.EqualTo("end"));

        ex = Assert.Throws<ArgumentOutOfRangeException>(() => new TagNS(5, 4));
        Assert.That(ex.ParamName, Is.EqualTo("end"));
    }
}