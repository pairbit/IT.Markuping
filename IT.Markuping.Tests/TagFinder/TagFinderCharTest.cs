#if NET
using IT.Markuping.Extensions;
using IT.Markuping.Interfaces;
using System.Linq;

namespace IT.Markuping.Tests;

internal class TagFinderCharTest
{
    [Test]
    public void Test()
    {
        Test(TagFinders.Char.Utf16_IgnoreCase);

        var tags = TagFinders.Char.Utf16_IgnoreCase.FirstTags("<NaMe></name>", "name");
        Assert.That(tags.IsEmpty, Is.False);
    }

    private static void Test(ITagFinder<char> finder)
    {
        var data = "<NaMe></NaMe>";
        var tags = finder.FirstTags(data, "name");
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);

        data = "<Ns:NaMe></Ns:NaMe>";
        tags = finder.FirstTags(data, "name", "ns");
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);
    }
}
#endif