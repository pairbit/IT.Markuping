namespace IT.Markuping;

public enum TagId
{
    Any = 0,
    id = 1,
    Id = 2,
    ID = 4,
    NoiD = id | Id | ID,
    iD = 8,

    WithoutPrefix = 16,
    WithPrefix = 32,
    xml = 64,

    xmlid = xml | id
}