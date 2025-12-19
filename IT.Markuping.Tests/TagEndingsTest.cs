using IT.Markuping.Extensions;

namespace IT.Markuping.Tests;

internal class TagEndingsTest
{
    [Test]
    public void Test()
    {
        var endings = TagEndings.AnyClosing;
        Assert.That(endings.IsValid(), Is.True);
        Assert.That(endings.IsAnyClosing(), Is.True);
        Assert.That(endings.HasNoAttributes(), Is.False);
        Assert.That(endings.HasAttributes(), Is.False);
        Assert.That(endings.HasClosing(), Is.False);
        Assert.That(endings.HasSelfClosing(), Is.False);

        endings = TagEndings.HasAttributes | TagEndings.HasNoAttributes;
        Assert.That(endings.IsValid(), Is.True);
        Assert.That(endings.IsAnyClosing(), Is.True);
        Assert.That(endings.HasNoAttributes(), Is.True);
        Assert.That(endings.HasAttributes(), Is.True);
        Assert.That(endings.HasClosing(), Is.False);
        Assert.That(endings.HasSelfClosing(), Is.False);

        endings = TagEndings.SelfClosingHasAttributes;
        Assert.That(endings.IsValid(), Is.True);
        Assert.That(endings.HasNoAttributes(), Is.False);
        Assert.That(endings.HasAttributes(), Is.True);
        Assert.That(endings.HasClosing(), Is.False);
        Assert.That(endings.HasSelfClosing(), Is.True);

        endings = (TagEndings)15;
        Assert.That(endings.IsValid(), Is.True);
        Assert.That(endings.IsAnyClosing(), Is.False);
        Assert.That(endings.HasNoAttributes(), Is.True);
        Assert.That(endings.HasAttributes(), Is.True);
        Assert.That(endings.HasClosing(), Is.True);
        Assert.That(endings.HasSelfClosing(), Is.True);

        endings = (TagEndings)16;
        Assert.That(endings.IsValid(), Is.False);
    }
}