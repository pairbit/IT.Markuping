using System;

namespace IT.Markuping.Tests;

internal class TagsTest
{
    [Test]
    public void CtorTest()
    {
        var tags = new Tags();
        Assert.That(tags.ToString(), Is.EqualTo("<0..0></0..0>"));

        tags = default;
        Assert.That(tags.ToString(), Is.EqualTo("<0..0></0..0>"));

        tags = new Tags(new(0, 1, false, false), new(1, 2));
        Assert.That(tags.ToString(), Is.EqualTo("<0..1></1..2>"));

        tags = new Tags(
            new(int.MaxValue - 2, int.MaxValue - 1, false, false),
            new(int.MaxValue - 1, int.MaxValue, hasSpace: true));
        Assert.That(tags.ToString(), Is.EqualTo("<2147483645..2147483646></2147483646..2147483647 >"));
    }

    [Test]
    public void TryFormatTest()
    {
        var tags = new Tags(new(0, 10, false, false), new(10, 11));
        Assert.That(tags.TryFormat(stackalloc char[12], out var written), Is.False);
        Assert.That(written == 0, Is.True);

        Assert.That(tags.TryFormat(stackalloc char[13], out written), Is.False);
        Assert.That(written == 0, Is.True);

        Assert.That(tags.TryFormat(stackalloc char[14], out written), Is.False);
        Assert.That(written == 0, Is.True);

        Span<char> span = stackalloc char[16];
        Assert.That(tags.TryFormat(span, out written), Is.True);
        Assert.That(written == 16, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("<0..10></10..11>"));

        //HasSpace closing
        tags = new Tags(new(0, 10, false, false), new(10, 11, true));
        Assert.That(tags.TryFormat(stackalloc char[13], out written), Is.False);
        Assert.That(written == 0, Is.True);

        Assert.That(tags.TryFormat(stackalloc char[14], out written), Is.False);
        Assert.That(written == 0, Is.True);

        Assert.That(tags.TryFormat(stackalloc char[16], out written), Is.False);
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
        var ex1 = Assert.Throws<ArgumentException>(() => new Tags(new(0, 1, false, isSelfClosing: true), default));
        Assert.That(ex1.ParamName, Is.EqualTo("opening"));
        Assert.That(ex1.Message, Is.EqualTo("SelfClosing (Parameter 'opening')"));

        var ex2 = Assert.Throws<ArgumentOutOfRangeException>(() => new Tags(default, default));
        Assert.That(ex2.ParamName, Is.EqualTo("opening"));
        Assert.That(ex2.Message, Is.EqualTo("Start >= End (Parameter 'opening')"));

        ex2 = Assert.Throws<ArgumentOutOfRangeException>(() => new Tags(new(0, 1, false, false), default));
        Assert.That(ex2.ParamName, Is.EqualTo("closing"));
        Assert.That(ex2.Message, Is.EqualTo("Start >= End (Parameter 'closing')"));

        ex2 = Assert.Throws<ArgumentOutOfRangeException>(() => new Tags(new(1, 2, false, false), new(1, 2)));
        Assert.That(ex2.ParamName, Is.EqualTo("closing"));
        Assert.That(ex2.Message, Is.EqualTo("openingEnd > closingStart (Parameter 'closing')"));
    }
}