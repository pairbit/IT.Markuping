using System;

namespace IT.Markuping.Tests;

internal class TagClosingTest
{
    [Test]
    public void Test()
    {
        var closing = new TagClosing();
        Assert.That(closing.Start, Is.EqualTo(0));
        Assert.That(closing.End, Is.EqualTo(0));
        Assert.That(closing.Length, Is.EqualTo(0));
        //Assert.That(closing.HasNamespace, Is.False);
        Assert.That(closing.HasSpace, Is.False);

        closing = default;
        Assert.That(closing.Start, Is.EqualTo(0));
        Assert.That(closing.End, Is.EqualTo(0));
        Assert.That(closing.Length, Is.EqualTo(0));
        //Assert.That(closing.HasNamespace, Is.False);
        Assert.That(closing.HasSpace, Is.False);

        closing = new(0, 1, hasSpace: false);
        Assert.That(closing.Start, Is.EqualTo(0));
        Assert.That(closing.End, Is.EqualTo(1));
        Assert.That(closing.Length, Is.EqualTo(1));
        //Assert.That(closing.HasNamespace, Is.True);
        Assert.That(closing.HasSpace, Is.False);

        closing = new(10, 50, hasSpace: true);
        Assert.That(closing.Start, Is.EqualTo(10));
        Assert.That(closing.End, Is.EqualTo(50));
        Assert.That(closing.Length, Is.EqualTo(40));
        //Assert.That(closing.HasNamespace, Is.False);
        Assert.That(closing.HasSpace, Is.True);

        closing = new(66, 77, hasSpace: true);
        Assert.That(closing.Start, Is.EqualTo(66));
        Assert.That(closing.End, Is.EqualTo(77));
        Assert.That(closing.Length, Is.EqualTo(11));
        //Assert.That(closing.HasNamespace, Is.True);
        Assert.That(closing.HasSpace, Is.True);
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