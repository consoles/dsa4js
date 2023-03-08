package io.github.ch8;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.time.temporal.ChronoUnit;
import java.util.Comparator;

public class App {
    public static void main(String[] args) {
        // ISO-8601
        System.out.println(Instant.now()); // Zulu Time UTC+0
        System.out.println(LocalDate.now());
        System.out.println(LocalTime.now());
        System.out.println(LocalDateTime.now());
        System.out.println(ZonedDateTime.now());

        LocalDateTime dateTime = LocalDateTime.of(2017, Month.JULY, 4, 13, 20, 10);
        ZonedDateTime nyc = dateTime.atZone(ZoneId.of("Asia/Shanghai"));
        System.out.println(nyc);
        ZonedDateTime london = nyc.withZoneSameInstant(ZoneId.of("Europe/London"));
        System.out.println(london);

        String text = LocalDateTime.now()
            .format(DateTimeFormatter.ISO_DATE_TIME);
        System.out.println(text);

        // 查找每个时区 ID 的偏移量
        Instant instant = Instant.now();
        ZonedDateTime current = instant.atZone(ZoneId.systemDefault());
        System.out.printf("Current time is %s%n%n", current);
        System.out.printf("%10s %20s %13s%n", "Offset", "ZoneId", "Time");
        ZoneId.getAvailableZoneIds().stream()
            .map(ZoneId::of)
//            .filter(zoneId -> {
//                ZoneOffset offset = instant.atZone(zoneId).getOffset();
//                return offset.getTotalSeconds() % (60 * 60) != 0;
//            })
            .sorted(Comparator.comparingInt(zoneId -> instant.atZone(zoneId).getOffset().getTotalSeconds()))
            .forEach(zoneId -> {
                ZonedDateTime zonedDateTime = current.withZoneSameInstant(zoneId);
                System.out.printf("%10s %25s %10s%n",
                    zonedDateTime.getOffset(),
                    zoneId,
                    zonedDateTime.format(DateTimeFormatter.ofLocalizedTime(FormatStyle.SHORT))
                );
            });

        LocalDate today = LocalDate.now();
        LocalDate laodongDay = LocalDate.of(2023, Month.MAY, 1);
        System.out.printf("距离2023-05-01 %s 天%n", ChronoUnit.DAYS.between(today, laodongDay));
    }
}
