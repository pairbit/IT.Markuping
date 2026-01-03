using IT.Markuping.Extensions;
using IT.Markuping.Interfaces;
using System;
using System.Diagnostics;

namespace IT.Markuping.Implementation;

internal class MarkupFinder<T> : IMarkupFinder<T> where T : unmanaged
{
    private readonly ITagStrategy<T> _strategy;

    public MarkupFinder(ITagStrategy<T> strategy)
    {
        _strategy = strategy ?? throw new ArgumentNullException(nameof(strategy));
    }

    #region IMarkupFinder

    public Tags FirstTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns, out int nodes)
    {
        throw new NotImplementedException();
        //var tagFinder = new ValueTagFinder<T>(data, name, _strategy);
        //var opening = tagFinder.FirstTag(out ns, TagEndings.Closing);
        //if (!opening.IsEmpty)
        //{
        //    int offset = opening.End;
        //    tagFinder.Data = data.Slice(offset);
        //    var closing = FirstClosing(data, name, data.Slice(ns.Start, ns.Length), , out nodes);
        //    if (!closing.IsEmpty)
        //    {
        //        return new(opening, closing);
        //    }
        //}
        //nodes = default;
        //return default;
    }

    public Tags FirstTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, out int nodes)
    {
        throw new NotImplementedException();
        //var opening = FirstTag(data, name, ns, TagEndings.Closing);
        //if (!opening.IsEmpty)
        //{
        //    var closing = FirstClosing(data, name, ns, opening.End, out nodes);
        //    if (!closing.IsEmpty)
        //    {
        //        return new(opening, closing);
        //    }
        //}
        //nodes = default;
        //return default;
    }

    public Tags FirstTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes)
    {
        throw new NotImplementedException();
        //var opening = FirstTag(data, name, TagEndings.Closing);
        //if (!opening.IsEmpty)
        //{
        //    var closing = FirstClosing(data, name, default, opening.End, out nodes);
        //    if (!closing.IsEmpty)
        //    {
        //        return new(opening, closing);
        //    }
        //}
        //nodes = default;
        //return default;
    }

    public Tags LastTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns, out int nodes)
    {
        throw new NotImplementedException();
        //var closing = LastTagClosing(data, name, out ns);
        //if (!closing.IsEmpty)
        //{
        //    var opening = LastOpening(data.Slice(0, closing.Start), name, data.Slice(ns.Start, ns.Length), out nodes);
        //    if (!opening.IsEmpty)
        //    {
        //        return new(opening, closing, nodes > 0);
        //    }
        //}
        //nodes = default;
        //ns = default;
        //return default;
    }

    public Tags LastTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, out int nodes)
    {
        throw new NotImplementedException();
        //var closing = LastTagClosing(data, name, ns);
        //if (!closing.IsEmpty)
        //{
        //    var opening = LastOpening(data.Slice(0, closing.Start), name, ns, out nodes);
        //    if (!opening.IsEmpty)
        //    {
        //        return new(opening, closing, nodes > 0);
        //    }
        //}
        //nodes = default;
        //return default;
    }

    public Tags LastTags(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes)
    {
        throw new NotImplementedException();
        //var closing = LastTagClosing(data, name);
        //if (!closing.IsEmpty)
        //{
        //    var opening = LastOpening(data.Slice(0, closing.Start), name, default, out nodes);
        //    if (!opening.IsEmpty)
        //    {
        //        return new(opening, closing, nodes > 0);
        //    }
        //}
        //nodes = default;
        //return default;
    }

    public Tag FirstTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns, TagEndings endings = default)
        => new ValueTagFinder<T>(data, name, _strategy).FirstTag(out ns, endings);

    public Tag FirstTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default)
        => new ValueTagFinder<T>(data, name, _strategy).FirstTag(ns, endings);

    public Tag FirstTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default)
        => new ValueTagFinder<T>(data, name, _strategy).FirstTag(endings);

    public Tag LastTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns, TagEndings endings = default)
        => new ValueTagFinder<T>(data, name, _strategy).LastTag(out ns, endings);

    public Tag LastTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default)
        => new ValueTagFinder<T>(data, name, _strategy).LastTag(ns, endings);

    public Tag LastTag(ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default)
        => new ValueTagFinder<T>(data, name, _strategy).LastTag(endings);

    public TagClosing FirstTagClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns)
        => new ValueTagFinder<T>(data, name, _strategy).FirstTagClosing(ns);

    public TagClosing FirstTagClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name)
        => new ValueTagFinder<T>(data, name, _strategy).FirstTagClosing();

    public TagClosing LastTagClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out TagNS ns)
        => new ValueTagFinder<T>(data, name, _strategy).LastTagClosing(out ns);

    public TagClosing LastTagClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns)
        => new ValueTagFinder<T>(data, name, _strategy).LastTagClosing(ns);

    public TagClosing LastTagClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name)
        => new ValueTagFinder<T>(data, name, _strategy).LastTagClosing();

    #endregion IMarkupFinder
}