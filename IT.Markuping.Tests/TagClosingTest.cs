using System;

namespace IT.Markuping.Tests;

internal class TagClosingTest
{
    [Test]
    public void CtorTest()
    {
        var closing = new TagClosing();
        Assert.That(closing.Start, Is.EqualTo(0));
        Assert.That(closing.End, Is.EqualTo(0));
        Assert.That(closing.Length, Is.EqualTo(0));
        Assert.That(closing.IsTree, Is.False);
        Assert.That(closing.HasSpace, Is.False);
        Assert.That(closing.ToString(), Is.EqualTo("</0..0>"));

        closing = default;
        Assert.That(closing.Start, Is.EqualTo(0));
        Assert.That(closing.End, Is.EqualTo(0));
        Assert.That(closing.Length, Is.EqualTo(0));
        Assert.That(closing.IsTree, Is.False);
        Assert.That(closing.HasSpace, Is.False);

        closing = new(0, 1, hasSpace: false);
        Assert.That(closing.Start, Is.EqualTo(0));
        Assert.That(closing.End, Is.EqualTo(1));
        Assert.That(closing.Length, Is.EqualTo(1));
        Assert.That(closing.IsTree, Is.False);
        Assert.That(closing.HasSpace, Is.False);

        closing = new(10, 50, hasSpace: true);
        Assert.That(closing.Start, Is.EqualTo(10));
        Assert.That(closing.End, Is.EqualTo(50));
        Assert.That(closing.Length, Is.EqualTo(40));
        Assert.That(closing.IsTree, Is.False);
        Assert.That(closing.HasSpace, Is.True);

        closing = new(66, 77, hasSpace: true);
        Assert.That(closing.Start, Is.EqualTo(66));
        Assert.That(closing.End, Is.EqualTo(77));
        Assert.That(closing.Length, Is.EqualTo(11));
        Assert.That(closing.IsTree, Is.False);
        Assert.That(closing.HasSpace, Is.True);
        Assert.That(closing.ToString(), Is.EqualTo("</66..77 >"));

        closing = new(int.MaxValue - 1, int.MaxValue, hasSpace: true);
        closing = closing.AsTree();
        Assert.That(closing.Start, Is.EqualTo(2147483646));
        Assert.That(closing.End, Is.EqualTo(2147483647));
        Assert.That(closing.Length, Is.EqualTo(1));
        Assert.That(closing.IsTree, Is.True);
        Assert.That(closing.HasSpace, Is.True);
        Assert.That(closing.ToString(), Is.EqualTo("</2147483646..2147483647 >"));
    }

#if NET
    [Test]
    public void TryFormatTest()
    {
        var closing = new TagClosing(10, 11);

        Assert.That(closing.TryFormat(stackalloc char[6], out var written), Is.False);
        Assert.That(written == 0, Is.True);

        Assert.That(closing.TryFormat(stackalloc char[7], out written), Is.False);
        Assert.That(written == 0, Is.True);

        Span<char> span = stackalloc char[9];
        Assert.That(closing.TryFormat(span, out written), Is.True);
        Assert.That(written == 9, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("</10..11>"));

        closing = new TagClosing(12345, 123456);
        Assert.That(closing.TryFormat(stackalloc char[15], out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[16];
        Assert.That(closing.TryFormat(span, out written), Is.True);
        Assert.That(written == 16, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("</12345..123456>"));

        //space
        closing = new TagClosing(10, 11, hasSpace: true);

        Assert.That(closing.TryFormat(stackalloc char[7], out written), Is.False);
        Assert.That(written == 0, Is.True);

        Assert.That(closing.TryFormat(stackalloc char[8], out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[10];
        Assert.That(closing.TryFormat(span, out written), Is.True);
        Assert.That(written == 10, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("</10..11 >"));

        closing = new TagClosing(12345, 123456, hasSpace: true);
        Assert.That(closing.TryFormat(stackalloc char[16], out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[17];
        Assert.That(closing.TryFormat(span, out written), Is.True);
        Assert.That(written == 17, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("</12345..123456 >"));
    }
#endif

    [Test]
    public void CompareToTest()
    {
        var tag1 = new TagClosing(100, 101);
        var tag2 = new TagClosing(101, 102, true);

        Assert.That(tag1 < tag2, Is.True);
        Assert.That(tag2 > tag1, Is.True);

        tag2 = tag1;
        Assert.That(tag1 <= tag2, Is.True);
        Assert.That(tag1 >= tag2, Is.True);
    }

    [Test]
    public void InvalidTest()
    {
        var ex = Assert.Throws<ArgumentOutOfRangeException>(() => new TagClosing(-1, 0));
        Assert.That(ex.ParamName, Is.EqualTo("start"));

        ex = Assert.Throws<ArgumentOutOfRangeException>(() => new TagClosing(0, 0));
        Assert.That(ex.ParamName, Is.EqualTo("end"));

        ex = Assert.Throws<ArgumentOutOfRangeException>(() => new TagClosing(5, 4));
        Assert.That(ex.ParamName, Is.EqualTo("end"));
    }
}