using System;

namespace IT.Markuping.Tests;

internal class TagsTest
{
    [Test]
    public void CtorTest()
    {
        var tags = new Tags();
        Assert.That(tags.IsTree, Is.False);
        Assert.That(tags.HasAttributes, Is.False);
        Assert.That(tags.HasSpaceAtEnd, Is.False);
        Assert.That(tags.ToString(), Is.EqualTo("<0..0></0..0>"));

        tags = default;
        Assert.That(tags.IsTree, Is.False);
        Assert.That(tags.HasAttributes, Is.False);
        Assert.That(tags.HasSpaceAtEnd, Is.False);
        Assert.That(tags.ToString(), Is.EqualTo("<0..0></0..0>"));

        tags = new Tags(new(0, 1), new(1, 2));
        Assert.That(tags.IsTree, Is.False);
        Assert.That(tags.HasAttributes, Is.False);
        Assert.That(tags.HasSpaceAtEnd, Is.False);
        Assert.That(tags.ToString(), Is.EqualTo("<0..1></1..2>"));

        tags = new Tags(new(1, 2, hasAttributes: true), new(10, 20));
        Assert.That(tags.IsTree, Is.False);
        Assert.That(tags.HasAttributes, Is.True);
        Assert.That(tags.HasSpaceAtEnd, Is.False);
        Assert.That(tags.ToString(), Is.EqualTo("<1..2></10..20>"));

        tags = new Tags(
            new(int.MaxValue - 2, int.MaxValue - 1),
            new(int.MaxValue - 1, int.MaxValue, hasSpace: true));
        Assert.That(tags.IsTree, Is.False);
        Assert.That(tags.HasAttributes, Is.False);
        Assert.That(tags.HasSpaceAtEnd, Is.True);
        Assert.That(tags.ToString(), Is.EqualTo("<2147483645..2147483646></2147483646..2147483647 >"));

        var closing = new TagClosing(10, 20, hasSpace: false, isTree: true);
        tags = new Tags(new(1, 2), closing);
        Assert.That(tags.IsTree, Is.True);
        Assert.That(tags.HasAttributes, Is.False);
        Assert.That(tags.HasSpaceAtEnd, Is.False);
        Assert.That(tags.Closing, Is.EqualTo(closing));
        Assert.That(tags.ToString(), Is.EqualTo("<1..2></10..20>"));

        closing = new TagClosing(11, 22);
        tags = new Tags(new(5, 10), closing, isTree: true);
        Assert.That(tags.IsTree, Is.True);
        Assert.That(tags.HasAttributes, Is.False);
        Assert.That(tags.HasSpaceAtEnd, Is.False);
        Assert.That(tags.Closing, Is.Not.EqualTo(closing));
        Assert.That(tags.ToString(), Is.EqualTo("<5..10></11..22>"));

        Assert.That(tags.Closing.IsTree, Is.True);
        tags = new Tags(new(6, 10), tags.Closing, isTree: true);
        Assert.That(tags.IsTree, Is.True);
        Assert.That(tags.HasAttributes, Is.False);
        Assert.That(tags.HasSpaceAtEnd, Is.False);
        Assert.That(tags.ToString(), Is.EqualTo("<6..10></11..22>"));
    }

    [Test]
    public void TryFormatTest()
    {
        var tags = new Tags(new(0, 10, false, false), new(10, 11));
        Assert.That(tags.TryFormat(stackalloc char[12], out var written), Is.False);
        Assert.That(written == 0, Is.True);

        Span<char> span = stackalloc char[13];
        Assert.That(tags.TryFormat(span, out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[14];
        Assert.That(tags.TryFormat(span, out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[16];
        Assert.That(tags.TryFormat(span, out written), Is.True);
        Assert.That(written == 16, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("<0..10></10..11>"));

        //HasSpace closing
        tags = new Tags(new(0, 10, false, false), new(10, 11, true));
        span = stackalloc char[13];
        Assert.That(tags.TryFormat(span, out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[14];
        Assert.That(tags.TryFormat(span, out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[16];
        Assert.That(tags.TryFormat(span, out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[17];
        Assert.That(tags.TryFormat(span, out written), Is.True);
        Assert.That(written == 17, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("<0..10></10..11 >"));
    }

    [Test]
    public void CompareToTest()
    {
        var tags1 = new Tags(new(0, 1, false, false), new(1, 2));
        var tags2 = new Tags(new(2, 3, false, false), new(4, 5));

        Assert.That(tags1 < tags2, Is.True);
        Assert.That(tags2 > tags1, Is.True);

        tags2 = tags1;
        Assert.That(tags1 <= tags2, Is.True);
        Assert.That(tags1 >= tags2, Is.True);
    }

    [Test]
    public void InvalidTest()
    {
        var ex1 = Assert.Throws<ArgumentException>(() => new Tags(new(0, 1, isSelfClosing: true), default));
        Assert.That(ex1.ParamName, Is.EqualTo("opening"));
        Assert.That(ex1.Message, Is.EqualTo("SelfClosing (Parameter 'opening')"));

        var ex2 = Assert.Throws<ArgumentOutOfRangeException>(() => new Tags(default, default));
        Assert.That(ex2.ParamName, Is.EqualTo("opening"));
        Assert.That(ex2.Message, Is.EqualTo("Start >= End (Parameter 'opening')"));

        ex2 = Assert.Throws<ArgumentOutOfRangeException>(() => new Tags(new(0, 1), default));
        Assert.That(ex2.ParamName, Is.EqualTo("closing"));
        Assert.That(ex2.Message, Is.EqualTo("Start >= End (Parameter 'closing')"));

        ex2 = Assert.Throws<ArgumentOutOfRangeException>(() => new Tags(new(1, 2), new(1, 2)));
        Assert.That(ex2.ParamName, Is.EqualTo("closing"));
        Assert.That(ex2.Message, Is.EqualTo("openingEnd > closingStart (Parameter 'closing')"));

        ex1 = Assert.Throws<ArgumentException>(() => new Tags(new(0, 1), new(1, 2, false, isTree: true), isTree: false));
        Assert.That(ex1.ParamName, Is.EqualTo("closing"));
        Assert.That(ex1.Message, Is.EqualTo("Tree (Parameter 'closing')"));
    }
}