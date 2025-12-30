using System;

namespace IT.Markuping.Interfaces;

public interface ITagFinder<T> where T : unmanaged
{
    Tags FirstTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns, out int nodes);

    Tags FirstTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, out int nodes);

    Tags FirstTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes);

    Tags LastTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns, out int nodes);

    Tags LastTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, out int nodes);

    Tags LastTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes);

    Tag FirstTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns, TagEndings endings = default);

    Tag FirstTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default);

    Tag FirstTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default);

    Tag LastTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns, TagEndings endings = default);

    Tag LastTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default);

    Tag LastTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default);

    TagClosing FirstTagClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns);

    TagClosing FirstTagClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name);

    TagClosing LastTagClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns);

    TagClosing LastTagClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns);

    TagClosing LastTagClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name);

    //? LastAttrName(ReadOnlySpan<T> data, ReadOnlySpan<T> attrValue);
    //? LastAttrValue(ReadOnlySpan<T> data, ReadOnlySpan<T> attrName);
}