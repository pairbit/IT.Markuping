using System;

namespace IT.Markuping.Tests;

internal class TagTest
{
    [Test]
    public void Test()
    {
        var tag = new Tag();
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(0));
        Assert.That(tag.Length, Is.EqualTo(0));
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.None));
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.None));
        Assert.That(tag.Unended, Is.EqualTo(TagEnding.None));
        Assert.That(tag.TryGetOpening(out var opening), Is.False);
        Assert.That(opening.IsEmpty, Is.True);
        Assert.That(tag.TryGetOpening(out opening), Is.False);
        Assert.That(opening.IsEmpty, Is.True);
        Assert.That(tag.ToString(), Is.EqualTo("<0..0>"));

        tag = default;
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(0));
        Assert.That(tag.Length, Is.EqualTo(0));
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.None));
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.None));
        Assert.That(tag.Unended, Is.EqualTo(TagEnding.None));
        Assert.That(tag.TryGetOpening(out opening), Is.False);
        Assert.That(opening.IsEmpty, Is.True);
        Assert.That(tag.TryGetOpening(out opening), Is.False);
        Assert.That(opening.IsEmpty, Is.True);

        tag = new Tag(0, 1, TagEnding.Closing);
        Assert.That(tag.Start, Is.EqualTo(0));
        Assert.That(tag.End, Is.EqualTo(1));
        Assert.That(tag.Length, Is.EqualTo(1));
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.Closing));
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.Closing));
        Assert.That(tag.Unended, Is.EqualTo(TagEnding.Closing));
        Assert.That(tag.ToString(), Is.EqualTo("<0..1>"));
        opening = (TagOpening)tag;
        Assert.That(opening.HasAttributes, Is.False);
        Assert.That(opening.IsSelfClosing, Is.False);
        Assert.That(opening.Start, Is.EqualTo(tag.Start));
        Assert.That(opening.End, Is.EqualTo(tag.End));
        Assert.That((Tag)opening, Is.EqualTo(tag));
        Assert.That(tag.TryGetOpening(out opening), Is.True);
        Assert.That(opening.HasAttributes, Is.False);
        Assert.That(opening.IsSelfClosing, Is.False);
        Assert.That(opening.Start, Is.EqualTo(tag.Start));
        Assert.That(opening.End, Is.EqualTo(tag.End));
        Assert.That((Tag)opening, Is.EqualTo(tag));

        tag = new Tag(10, 100, TagEnding.SelfClosing);
        Assert.That(tag.Start, Is.EqualTo(10));
        Assert.That(tag.End, Is.EqualTo(100));
        Assert.That(tag.Length, Is.EqualTo(90));
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(tag.Unended, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(tag.ToString(), Is.EqualTo("<10..100>"));
        opening = (TagOpening)tag;
        Assert.That(opening.HasAttributes, Is.False);
        Assert.That(opening.IsSelfClosing, Is.True);
        Assert.That(opening.Start, Is.EqualTo(tag.Start));
        Assert.That(opening.End, Is.EqualTo(tag.End));
        Assert.That((Tag)opening, Is.EqualTo(tag));
        Assert.That(tag.TryGetOpening(out opening), Is.True);
        Assert.That(opening.HasAttributes, Is.False);
        Assert.That(opening.IsSelfClosing, Is.True);
        Assert.That(opening.Start, Is.EqualTo(tag.Start));
        Assert.That(opening.End, Is.EqualTo(tag.End));
        Assert.That((Tag)opening, Is.EqualTo(tag));

        tag = new Tag(5, 10, TagEnding.Name);
        Assert.That(tag.Start, Is.EqualTo(5));
        Assert.That(tag.End, Is.EqualTo(10));
        Assert.That(tag.Length, Is.EqualTo(5));
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.None));
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(tag.Unended, Is.EqualTo(TagEnding.Name));
        Assert.That(tag.ToString(), Is.EqualTo("<5..10>"));
        opening = (TagOpening)tag;
        Assert.That(opening.HasAttributes, Is.True);
        Assert.That(opening.IsSelfClosing, Is.False);
        Assert.That(opening.Start, Is.EqualTo(tag.Start));
        Assert.That(opening.End, Is.EqualTo(tag.End));
        Assert.That((Tag)opening, Is.EqualTo(tag));
        Assert.That(tag.TryGetOpening(out opening), Is.False);
        Assert.That(opening.IsEmpty, Is.True);

        tag = new Tag(1, 3, TagEnding.ClosingHasAttributes);
        Assert.That(tag.Start, Is.EqualTo(1));
        Assert.That(tag.End, Is.EqualTo(3));
        Assert.That(tag.Length, Is.EqualTo(2));
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.None));
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(tag.Unended, Is.EqualTo(TagEnding.Name));
        opening = (TagOpening)tag;
        Assert.That(opening.HasAttributes, Is.True);
        Assert.That(opening.IsSelfClosing, Is.False);
        Assert.That(opening.Start, Is.EqualTo(tag.Start));
        Assert.That(opening.End, Is.EqualTo(tag.End));
        Assert.That((Tag)opening, Is.EqualTo(tag));
        Assert.That(tag.TryGetOpening(out opening), Is.False);
        Assert.That(opening.IsEmpty, Is.True);

        tag = new Tag(1, 3, TagEnding.SelfClosingHasAttributes);
        Assert.That(tag.Start, Is.EqualTo(1));
        Assert.That(tag.End, Is.EqualTo(3));
        Assert.That(tag.Length, Is.EqualTo(2));
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.None));
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.SelfClosingHasAttributes));
        Assert.That(tag.Unended, Is.EqualTo(TagEnding.AttributeStart));
        opening = (TagOpening)tag;
        Assert.That(opening.HasAttributes, Is.True);
        Assert.That(opening.IsSelfClosing, Is.True);
        Assert.That(opening.Start, Is.EqualTo(tag.Start));
        Assert.That(opening.End, Is.EqualTo(tag.End));
        Assert.That((Tag)opening, Is.EqualTo(tag));
        Assert.That(tag.TryGetOpening(out opening), Is.False);
        Assert.That(opening.IsEmpty, Is.True);

        tag = new Tag(1, 3, TagEnding.AttributeStart);
        Assert.That(tag.Start, Is.EqualTo(1));
        Assert.That(tag.End, Is.EqualTo(3));
        Assert.That(tag.Length, Is.EqualTo(2));
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.None));
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.SelfClosingHasAttributes));
        Assert.That(tag.Unended, Is.EqualTo(TagEnding.AttributeStart));
        opening = (TagOpening)tag;
        Assert.That(opening.HasAttributes, Is.True);
        Assert.That(opening.IsSelfClosing, Is.True);
        Assert.That(opening.Start, Is.EqualTo(tag.Start));
        Assert.That(opening.End, Is.EqualTo(tag.End));
        Assert.That((Tag)opening, Is.EqualTo(tag));
        Assert.That(tag.TryGetOpening(out opening), Is.False);
        Assert.That(opening.IsEmpty, Is.True);
    }

    [Test]
    public void TryFormatTest()
    {
        var tag = new Tag(10, 11, TagEnding.Closing);
        Assert.That(tag.TryFormat(stackalloc char[5], out var written), Is.False);
        Assert.That(written == 0, Is.True);

        Assert.That(tag.TryFormat(stackalloc char[6], out written), Is.False);
        Assert.That(written == 0, Is.True);

        Span<char> span = stackalloc char[8];
        Assert.That(tag.TryFormat(span, out written), Is.True);
        Assert.That(written == 8, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("<10..11>"));

        tag = new Tag(12345, 123456, TagEnding.Closing);
        Assert.That(tag.TryFormat(stackalloc char[14], out written), Is.False);
        Assert.That(written == 0, Is.True);

        span = stackalloc char[15];
        Assert.That(tag.TryFormat(span, out written), Is.True);
        Assert.That(written == 15, Is.True);
        Assert.That(span.ToString(), Is.EqualTo("<12345..123456>"));
    }

    [Test]
    public void CompareToTest()
    {
        var tag1 = new Tag(100, 101, TagEnding.Closing);
        var tag2 = new Tag(101, 102, TagEnding.Closing);

        Assert.That(tag1 < tag2, Is.True);
        Assert.That(tag2 > tag1, Is.True);

        tag2 = tag1;
        Assert.That(tag1 <= tag2, Is.True);
        Assert.That(tag1 >= tag2, Is.True);
    }

    [Test]
    public void InvalidTest()
    {
        var ex = Assert.Throws<ArgumentOutOfRangeException>(() => new Tag(-1, 0, default));
        Assert.That(ex.ParamName, Is.EqualTo("start"));

        ex = Assert.Throws<ArgumentOutOfRangeException>(() => new Tag(0, 0, default));
        Assert.That(ex.ParamName, Is.EqualTo("end"));

        ex = Assert.Throws<ArgumentOutOfRangeException>(() => new Tag(5, 4, default));
        Assert.That(ex.ParamName, Is.EqualTo("end"));

        ex = Assert.Throws<ArgumentOutOfRangeException>(() => new Tag(5, 6, default));
        Assert.That(ex.ParamName, Is.EqualTo("ending"));
    }
}