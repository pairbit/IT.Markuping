using System;

namespace IT.Markuping.Tests;

internal class TagOpeningTest
{
    [Test]
    public void CtorTest()
    {
        var tag = new TagOpening();
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(0));
        Assert.That(tag.Length, Is.EqualTo(0));
        Assert.That(tag.HasAttributes, Is.False);
        Assert.That(tag.IsSelfClosing, Is.False);
        Assert.That(tag.ToString(), Is.EqualTo("<0..0>"));

        tag = default;
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(0));
        Assert.That(tag.Length, Is.EqualTo(0));
        Assert.That(tag.HasAttributes, Is.False);
        Assert.That(tag.IsSelfClosing, Is.False);

        tag = new TagOpening(0, 1, hasAttributes: false, isSelfClosing: true);
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(1));
        Assert.That(tag.Length, Is.EqualTo(1));
        Assert.That(tag.HasAttributes, Is.False);
        Assert.That(tag.IsSelfClosing, Is.True);
        Assert.That(tag.ToString(), Is.EqualTo("<0..1/>"));

        tag = new TagOpening(10, 100, hasAttributes: true, isSelfClosing: true);
        Assert.That(tag.Start, Is.EqualTo(10));
        Assert.That(tag.End, Is.EqualTo(100));
        Assert.That(tag.Length, Is.EqualTo(90));
        Assert.That(tag.HasAttributes, Is.True);
        Assert.That(tag.IsSelfClosing, Is.True);
        Assert.That(tag.ToString(), Is.EqualTo("<10..100/>"));

        tag = new TagOpening(5, 10, hasAttributes: true, isSelfClosing: false);
        Assert.That(tag.Start, Is.EqualTo(5));
        Assert.That(tag.End, Is.EqualTo(10));
        Assert.That(tag.Length, Is.EqualTo(5));
        Assert.That(tag.HasAttributes, Is.True);
        Assert.That(tag.IsSelfClosing, Is.False);
        Assert.That(tag.ToString(), Is.EqualTo("<5..10>"));

        tag = new TagOpening(1, 3, hasAttributes: false, isSelfClosing: false);
        Assert.That(tag.Start, Is.EqualTo(1));
        Assert.That(tag.End, Is.EqualTo(3));
        Assert.That(tag.HasAttributes, Is.False);
        Assert.That(tag.IsSelfClosing, Is.False);
        Assert.That(tag.ToString(), Is.EqualTo("<1..3>"));
    }

    [Test]
    public void AddOffsetTest()
    {
        var tag = new TagOpening(0, 1, hasAttributes: false, isSelfClosing: false);
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

        tag = new TagOpening(0, 1, hasAttributes: true, isSelfClosing: true);
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

    [Test]
    public void TryFormatTest()
    {
        var tag = new TagOpening(10, 11, hasAttributes: false, isSelfClosing: false);
        Assert.That(tag.TryFormat(stackalloc char[5], out var written), Is.False);
        Assert.That(written == 0, Is.True);

        Assert.That(tag.TryFormat(stackalloc char[6], out written), Is.False);
        Assert.That(written == 0, Is.True);

        Span<char> span = stackalloc char[8];
        Assert.That(tag.TryFormat(span, out written), Is.True);
        Assert.That(written == 8, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("<10..11>"));

        tag = new TagOpening(12345, 123456, hasAttributes: false, isSelfClosing: false);
        Assert.That(tag.TryFormat(stackalloc char[14], out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[15];
        Assert.That(tag.TryFormat(span, out written), Is.True);
        Assert.That(written == 15, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("<12345..123456>"));

        //SelfClosing
        tag = new TagOpening(10, 11, hasAttributes: false, isSelfClosing: true);
        Assert.That(tag.TryFormat(stackalloc char[6], out written), Is.False);
        Assert.That(written == 0, Is.True);

        Assert.That(tag.TryFormat(stackalloc char[7], out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[9];
        Assert.That(tag.TryFormat(span, out written), Is.True);
        Assert.That(written == 9, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("<10..11/>"));

        tag = new TagOpening(12345, 123456, hasAttributes: false, isSelfClosing: true);
        Assert.That(tag.TryFormat(stackalloc char[15], out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[16];
        Assert.That(tag.TryFormat(span, out written), Is.True);
        Assert.That(written == 16, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("<12345..123456/>"));
    }

    [Test]
    public void CompareToTest()
    {
        var tag1 = new TagOpening(100, 101, false, false);
        var tag2 = new TagOpening(101, 102, false, false);

        Assert.That(tag1 < tag2, Is.True);
        Assert.That(tag2 > tag1, Is.True);

        tag2 = tag1;
        Assert.That(tag1 <= tag2, Is.True);
        Assert.That(tag1 >= tag2, Is.True);
    }

    [Test]
    public void InvalidTest()
    {
        var ex = Assert.Throws<ArgumentOutOfRangeException>(() => new TagOpening(-1, 0, default, default));
        Assert.That(ex.ParamName, Is.EqualTo("start"));

        ex = Assert.Throws<ArgumentOutOfRangeException>(() => new TagOpening(0, 0, default, default));
        Assert.That(ex.ParamName, Is.EqualTo("end"));

        ex = Assert.Throws<ArgumentOutOfRangeException>(() => new TagOpening(5, 4, default, default));
        Assert.That(ex.ParamName, Is.EqualTo("end"));
    }
}