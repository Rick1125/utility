-module(datetime).

-export([greenwich_mean_time/0, greenwich_mean_time/1]).

greenwich_mean_time() ->
    greenwich_mean_time(0).

greenwich_mean_time(OffSecond) ->
    Months = {"Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nove", "Dec"},
    Weekdays = {"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" },
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + OffSecond,
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(Seconds),
    Weekday = calendar:day_of_the_week({Year, Month, Day}),
    WeekdayName = element(Weekday, Weekdays),
    MonthName = element(Month, Months),
    lists:append([WeekdayName, io_lib:format(", ~2.2.0w ", [Day]), MonthName,
                 lists:flatten(io_lib:format(" ~w ~2.2.0w:~2.2.0w:~2.2.0w GMT", [Year, Hour, Minute, Second]))]).

