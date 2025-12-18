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

        tag = new TagOpening(10, 100, hasAttributes: true, isSelfClosing: true);
        Assert.That(tag.Start, Is.EqualTo(10));
        Assert.That(tag.End, Is.EqualTo(100));
        Assert.That(tag.Length, Is.EqualTo(90));
        Assert.That(tag.HasAttributes, Is.True);
        Assert.That(tag.IsSelfClosing, Is.True);

        tag = new TagOpening(5, 10, hasAttributes: true, isSelfClosing: false);
        Assert.That(tag.Start, Is.EqualTo(5));
        Assert.That(tag.End, Is.EqualTo(10));
        Assert.That(tag.Length, Is.EqualTo(5));
        Assert.That(tag.HasAttributes, Is.True);
        Assert.That(tag.IsSelfClosing, Is.False);

        tag = new TagOpening(1, 3, hasAttributes: false, isSelfClosing: false);
        Assert.That(tag.Start, Is.EqualTo(1));
        Assert.That(tag.End, Is.EqualTo(3));
        Assert.That(tag.HasAttributes, Is.False);
        Assert.That(tag.IsSelfClosing, Is.False);
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