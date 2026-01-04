#if NET
using IT.Markuping.Extensions;
using IT.Markuping.Interfaces;
using System.Linq;

namespace IT.Markuping.Tests;

internal class MarkupFinderCharTest
{
    [Test]
    public void Test()
    {
        Test(MarkupFinders.Char.Utf16_IgnoreCase);
    }

    private static void Test(IMarkupFinder<char> finder)
    {
        var data = "<NaMe></NaMe>";
        var tags = finder.FirstTags(data, "name");
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);

        data = "<NaMe></name>";
        tags = finder.FirstTags(data, "name");
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);

        data = "<Ns:NaMe></Ns:NaMe>";
        tags = finder.FirstTags(data, "name", "ns");
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);

        data = "<Ns:NaMe></NS:name>";
        tags = finder.FirstTags(data, "name", "ns");
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);
    }
}
#endif