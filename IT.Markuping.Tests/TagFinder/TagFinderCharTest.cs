using IT.Markuping.Implementation;
#if NET
using IT.Markuping.Interfaces;
using System.Collections.Generic;
using System.Linq;

namespace IT.Markuping.Tests;

internal class TagFinderCharTest
{
    [Test]
    public void Test()
    {
        Test(TagFinderChar.Utf16_IgnoreCase);

        var tags = TagFinderChar.Utf16_IgnoreCase.FirstPair("<NaMe></name>", "name", out _);
        Assert.That(tags.IsEmpty, Is.False);
    }

    private static void Test(ITagFinder<char> finder)
    {
        var data = "<NaMe></NaMe>";
        var tags = finder.FirstPair(data, "name", out var nodes);
        Assert.That(nodes, Is.Zero);
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);

        data = "<Ns:NaMe></Ns:NaMe>";
        tags = finder.FirstPair(data, "name", out nodes, "ns");
        Assert.That(nodes, Is.Zero);
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);
    }
}
#endif