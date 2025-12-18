using System;

namespace IT.Markuping.Interfaces;

public interface ITagFinder<T> where T : unmanaged
{
    //Tag FirstPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out Range ns);

    Tags LastPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out Range ns);

    //Tag First(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out Range ns, TagEndings endings = default);

    Tag First(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default);

    Tag First(ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default);

    Tag Last(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default);

    Tag Last(ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default);

    TagClosing LastClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out Range ns);
}