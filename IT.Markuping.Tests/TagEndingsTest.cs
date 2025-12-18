using IT.Markuping.Extensions;

namespace IT.Markuping.Tests;

internal class TagEndingsTest
{
    [Test]
    public void Test()
    {
        var endings = TagEndings.AnyClosing;
        Assert.That(endings.HasNoAttributes(), Is.False);
        Assert.That(endings.HasAttributes(), Is.False);
        Assert.That(endings.HasClosing(), Is.False);
        Assert.That(endings.HasSelfClosing(), Is.False);

        endings = TagEndings.SelfClosingHasAttributes;
        Assert.That(endings.HasNoAttributes(), Is.False);
        Assert.That(endings.HasAttributes(), Is.True);
        Assert.That(endings.HasClosing(), Is.False);
        Assert.That(endings.HasSelfClosing(), Is.True);
    }
}