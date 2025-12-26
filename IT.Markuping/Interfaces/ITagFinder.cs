using System;

namespace IT.Markuping.Interfaces;

public interface ITagFinder<T> where T : unmanaged
{
    Tags FirstPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes, out TagNS ns);

    Tags FirstPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes, ReadOnlySpan<T> ns);

    Tags FirstPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes);

    Tags LastPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes, out TagNS ns);

    Tags LastPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes, ReadOnlySpan<T> ns);

    Tags LastPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes);

    Tag First(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns, TagEndings endings = default);

    Tag First(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default);

    Tag First(ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default);

    //Tag Last(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns, TagEndings endings = default);

    Tag Last(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default);

    Tag Last(ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default);

    TagClosing FirstClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns);

    TagClosing FirstClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name);

    TagClosing LastClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns);

    TagClosing LastClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns);

    TagClosing LastClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name);
}