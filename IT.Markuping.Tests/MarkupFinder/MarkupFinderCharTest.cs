#if NET
using IT.Markuping.Extensions;
using IT.Markuping.Interfaces;
using System.Linq;
using System.Text;

namespace IT.Markuping.Tests;

internal class MarkupFinderCharTest
{
    [Test]
    public void Test()
    {
        Test(MarkupFinders.Char.Utf16_IgnoreCase);
        Test(MarkupFinders.Utf16_IgnoreCase, Encoding.Unicode);
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

    private static void Test(IMarkupFinder<byte> finder, Encoding encoding)
    {
        var name = encoding.GetBytes("name");
        var ns = encoding.GetBytes("ns");

        var data = encoding.GetBytes("<NaMe></NaMe>");
        var tags = finder.FirstTags(data, name);
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);

        data = encoding.GetBytes("<NaMe></name>");
        tags = finder.FirstTags(data, name);
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);

        data = encoding.GetBytes("<Ns:NaMe></Ns:NaMe>");
        tags = finder.FirstTags(data, name, ns);
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);

        data = encoding.GetBytes("<Ns:NaMe></NS:name>");
        tags = finder.FirstTags(data, name, ns);
        Assert.That(data[tags.Outer].SequenceEqual(data), Is.True);

        Assert.That(finder.FirstTags(data, encoding.GetBytes("invalid"), ns).IsEmpty, Is.True);
    }
}
#endif