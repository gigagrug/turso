use crate::types::AsValueRef;
use crate::types::Value;
use crate::LimboError::InvalidModifier;
use crate::{Result, ValueRef};
use chrono::{Local, Offset, TimeZone};
use std::fmt::Write;

const JD_TO_MS: i64 = 86_400_000;
const UNIX_EPOCH_JD: i64 = 21086676 * 10000; // 2440587.5 * 86400000
const MAX_JD: i64 = 464269060799999; // 9999-12-31 23:59:59.999
const LIMIT_SECOND: f64 = 4.6427e+14;
const LIMIT_MINUTE: f64 = 7.7379e+12;
const LIMIT_HOUR: f64 = 1.2897e+11;
const LIMIT_DAY: f64 = 5373485.0;
const LIMIT_MONTH: f64 = 176546.0;
const LIMIT_YEAR: f64 = 14713.0;

#[derive(Debug, Clone, Copy)]
struct DateTime {
    i_jd: i64, // The julian day number times 86400000
    y: i32,
    m: i32,
    d: i32,
    h: i32,
    min: i32,
    s: f64,
    tz: i32, // Timezone offset in minutes
    n_floor: i32,
    valid_jd: bool,
    valid_ymd: bool,
    valid_hms: bool,
    raw_s: bool, // Raw numeric value stored in s
    is_error: bool,
    use_subsec: bool,
    is_utc: bool,
    is_local: bool,
}

impl Default for DateTime {
    fn default() -> Self {
        DateTime {
            i_jd: 0,
            y: 2000,
            m: 1,
            d: 1,
            h: 0,
            min: 0,
            s: 0.0,
            tz: 0,
            n_floor: 0,
            valid_jd: false,
            valid_ymd: false,
            valid_hms: false,
            raw_s: false,
            is_error: false,
            use_subsec: false,
            is_utc: false,
            is_local: false,
        }
    }
}

impl DateTime {
    fn set_error(&mut self) {
        *self = DateTime::default();
        self.is_error = true;
    }

    fn compute_jd(&mut self) {
        if self.valid_jd {
            return;
        }

        let (y_in, m_in, d_in) = if self.valid_ymd {
            (self.y, self.m, self.d)
        } else {
            (2000, 1, 1)
        };

        if y_in < -4713 || y_in > 9999 || self.raw_s {
            self.set_error();
            return;
        }

        let mut y = y_in;
        let mut m = m_in;
        let d = d_in;

        if m <= 2 {
            y -= 1;
            m += 12;
        }

        let a = (y + 4800) / 100;
        let b = 38 - a + (a / 4);
        let x1 = 36525 * (y + 4716) / 100;
        let x2 = 306001 * (m + 1) / 10000;

        let day_part = (x1 + x2 + d + b - 1524) as i64;
        let day_ms = day_part * JD_TO_MS - 43200000;

        self.i_jd = day_ms;

        if self.valid_hms {
            let h_ms = self.h as i64 * 3_600_000;
            let m_ms = self.min as i64 * 60_000;
            let s_ms = (self.s * 1000.0 + 0.5) as i64;
            self.i_jd += h_ms + m_ms + s_ms;

            if self.tz != 0 {
                self.i_jd -= self.tz as i64 * 60_000;
                self.valid_ymd = false;
                self.valid_hms = false;
                self.tz = 0;
                self.is_utc = true;
                self.is_local = false;
            }
        }

        self.valid_jd = true;
    }

    fn compute_ymd(&mut self) {
        if self.valid_ymd {
            return;
        }
        if !self.valid_jd {
            self.y = 2000;
            self.m = 1;
            self.d = 1;
        } else if self.i_jd < 0 || self.i_jd > MAX_JD {
            self.set_error();
            return;
        } else {
            let z = ((self.i_jd + 43200000) / JD_TO_MS) as i32;
            let alpha = ((z as f64 + 32044.75) / 36524.25) as i32 - 52;
            let a = z + 1 + alpha - ((alpha + 100) / 4) + 25;
            let b = a + 1524;
            let c = ((b as f64 - 122.1) / 365.25) as i32;
            let d_calc = (36525 * (c & 32767)) / 100;
            let e = ((b - d_calc) as f64 / 30.6001) as i32;
            let x1 = (30.6001 * e as f64) as i32;

            self.d = b - d_calc - x1;
            self.m = if e < 14 { e - 1 } else { e - 13 };
            self.y = if self.m > 2 { c - 4716 } else { c - 4715 };
        }
        self.valid_ymd = true;
    }

    fn compute_hms(&mut self) {
        if self.valid_hms {
            return;
        }
        self.compute_jd();

        let jd = self.i_jd + 43200000;
        let day_ms = (jd % JD_TO_MS) as i32;
        let day_ms = if day_ms < 0 {
            day_ms + JD_TO_MS as i32
        } else {
            day_ms
        };

        self.s = (day_ms % 60000) as f64 / 1000.0;
        let day_min = day_ms / 60000;
        self.min = day_min % 60;
        self.h = day_min / 60;
        self.raw_s = false;
        self.valid_hms = true;
    }

    fn compute_ymd_hms(&mut self) {
        self.compute_ymd();
        self.compute_hms();
    }

    fn clear_ymd_hms_tz(&mut self) {
        self.valid_ymd = false;
        self.valid_hms = false;
        self.tz = 0;
    }

    fn compute_floor(&mut self) {
        if self.m > 12 || self.m < 1 || self.d > 31 || self.d < 1 {
            return;
        }
        if self.d <= 28 {
            self.n_floor = 0;
            return;
        }
        if (0x15aa & (1 << self.m)) != 0 {
            self.n_floor = 0;
        } else if self.m != 2 {
            self.n_floor = if self.d == 31 { 1 } else { 0 };
        } else {
            if is_leap(self.y) {
                self.n_floor = self.d - 29;
            } else {
                self.n_floor = self.d - 28;
            }
        }
    }
}

fn parse_modifier_ymd(z: &str) -> Option<(i32, i32, i32)> {
    let parts: Vec<&str> = z.split('-').collect();
    if parts.len() != 3 {
        return None;
    }
    let y = parts[0].parse::<i32>().ok()?;
    let m = parts[1].parse::<i32>().ok()?;
    let d = parts[2].parse::<i32>().ok()?;
    Some((y, m, d))
}

fn parse_date(value: &str, p: &mut DateTime) -> Result<()> {
    if parse_yyyy_mm_dd(value, p) {
        return Ok(());
    }
    if parse_hh_mm_ss(value, p) {
        return Ok(());
    }
    if value.eq_ignore_ascii_case("now") {
        set_to_current(p);
        return Ok(());
    }
    if let Ok(val) = value.parse::<f64>() {
        p.s = val;
        p.raw_s = true;
        if val >= 0.0 && val < 5373484.5 {
            p.i_jd = (val * JD_TO_MS as f64 + 0.5) as i64;
            p.valid_jd = true;
        }
        return Ok(());
    }
    Err(crate::LimboError::InvalidModifier("Parse Failed".into()))
}

fn set_to_current(p: &mut DateTime) {
    let now = std::time::SystemTime::now();
    let duration = now
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    const UNIX_EPOCH_IJD: i64 = 210866760000000;
    p.i_jd = UNIX_EPOCH_IJD + duration.as_millis() as i64;
    p.valid_jd = true;
    p.is_utc = true;
    p.is_local = false;
    p.clear_ymd_hms_tz();
}

fn parse_yyyy_mm_dd(mut z: &str, p: &mut DateTime) -> bool {
    let neg = if z.starts_with('-') {
        z = &z[1..];
        true
    } else {
        false
    };

    if z.len() < 10 || !z.as_bytes()[4].eq(&b'-') || !z.as_bytes()[7].eq(&b'-') {
        return false;
    }

    let y = match z[0..4].parse::<i32>() {
        Ok(v) => v,
        Err(_) => return false,
    };
    let m = match z[5..7].parse::<i32>() {
        Ok(v) => v,
        Err(_) => return false,
    };
    let d = match z[8..10].parse::<i32>() {
        Ok(v) => v,
        Err(_) => return false,
    };

    if m < 1 || m > 12 {
        return false;
    }
    if d < 1 || d > 31 {
        return false;
    }

    z = &z[10..];

    if z.is_empty() {
        p.valid_hms = false;
    } else {
        let first = z.chars().next().unwrap();
        // SQLite Requirement: Separator must start with space or 'T'
        if !first.is_whitespace() && first != 'T' && first != 't' {
            return false;
        }
        let mut rest = z.trim_start();
        if rest.starts_with('T') || rest.starts_with('t') {
            rest = &rest[1..];
        }
        rest = rest.trim_start();

        if !rest.is_empty() {
            if !parse_hh_mm_ss(rest, p) {
                return false;
            }
        } else {
            p.valid_hms = false;
        }
    }

    p.valid_jd = false;
    p.valid_ymd = true;
    p.y = if neg { -y } else { y };
    p.m = m;
    p.d = d;

    p.compute_floor();

    if p.tz != 0 {
        p.compute_jd();
    }
    true
}

fn parse_hh_mm_ss(mut z: &str, p: &mut DateTime) -> bool {
    z = z.trim();
    if z.len() < 5 {
        return false;
    }

    // Strict HH:MM check
    if !z.as_bytes()[0].is_ascii_digit()
        || !z.as_bytes()[1].is_ascii_digit()
        || z.as_bytes()[2] != b':'
        || !z.as_bytes()[3].is_ascii_digit()
        || !z.as_bytes()[4].is_ascii_digit()
    {
        return false;
    }

    let h = z[0..2].parse::<i32>().unwrap_or(0);
    let m = z[3..5].parse::<i32>().unwrap_or(0);
    let mut s = 0.0;

    z = &z[5..];

    // Optional :SS.FFF
    if z.starts_with(':') {
        // Must have at least :SS (3 chars)
        if z.len() < 3 {
            return false;
        }
        z = &z[1..];

        // Seconds must be exactly 2 digits
        if !z.as_bytes()[0].is_ascii_digit() || !z.as_bytes()[1].is_ascii_digit() {
            return false;
        }

        let s_int = z[0..2].parse::<f64>().unwrap_or(0.0);
        let mut s_frac = 0.0;

        let mut end = 2; // consume 2 digits for seconds

        // Optional fractional part
        if end < z.len() && z.as_bytes()[end] == b'.' {
            // Must have digit after dot to consume it as part of seconds
            if end + 1 < z.len() && z.as_bytes()[end + 1].is_ascii_digit() {
                let frac_start = end;
                end += 1;
                while end < z.len() && z.as_bytes()[end].is_ascii_digit() {
                    end += 1;
                }
                s_frac = z[frac_start..end].parse::<f64>().unwrap_or(0.0);

                // SQLite logic: Truncate to avoid sub-millisecond rounding issues
                if s_frac > 0.999 {
                    s_frac = 0.999;
                }
            }
        }

        s = s_int + s_frac;
        z = &z[end..];
    }

    if h > 24 || m > 59 || s >= 60.0 {
        return false;
    }

    p.h = h;
    p.min = m;
    p.s = s;
    p.valid_hms = true;

    // Check timezone. parse_timezone returns true on error.
    if parse_timezone(z, p) {
        return false;
    }

    true
}

fn parse_timezone(mut z: &str, p: &mut DateTime) -> bool {
    z = z.trim_start();
    p.tz = 0;

    if z.is_empty() {
        return false;
    }

    let c = z.chars().next().unwrap();
    let sgn = if c == '-' {
        -1
    } else if c == '+' {
        1
    } else if c == 'Z' || c == 'z' {
        z = &z[1..];
        // SQLite checks for trailing garbage after 'Z'
        z = z.trim_start();
        if !z.is_empty() {
            return true; // Error: trailing characters
        }
        p.is_utc = true;
        p.is_local = false;
        return false;
    } else {
        // Characters remaining that are not a timezone indicator (+/-/Z)
        // imply invalid garbage at the end of the date string.
        return true;
    };

    // We have +/-. Advance past sign.
    z = &z[1..];

    // SQLite requires strict HH:MM format (5 chars)
    if z.len() < 5 {
        return true;
    }

    // Parse HH
    if !z.as_bytes()[0].is_ascii_digit() || !z.as_bytes()[1].is_ascii_digit() {
        return true;
    }
    let h: i32 = z[0..2].parse().unwrap_or(0);

    // SQLite '20b' specifier limits Hours to 14
    if h > 14 {
        return true;
    }

    // Expect ':' separator
    if z.as_bytes()[2] != b':' {
        return true;
    }

    // Parse MM
    if !z.as_bytes()[3].is_ascii_digit() || !z.as_bytes()[4].is_ascii_digit() {
        return true;
    }
    let m: i32 = z[3..5].parse().unwrap_or(0);

    if m > 59 {
        return true;
    }

    z = &z[5..];

    p.tz = sgn * (h * 60 + m);
    if p.tz == 0 {
        p.is_utc = true;
        p.is_local = false;
    }

    // Check for trailing garbage after HH:MM
    z = z.trim_start();
    if !z.is_empty() {
        return true;
    }

    false
}

fn auto_adjust_date(p: &mut DateTime) {
    if !p.raw_s || p.valid_jd {
        p.raw_s = false;
    } else if p.s >= -210866760000.0 && p.s <= 253402300799.0 {
        let r = p.s * 1000.0 + 210866760000000.0;
        p.i_jd = (r + 0.5) as i64;
        p.valid_jd = true;
        p.raw_s = false;
        p.clear_ymd_hms_tz();
    }
}

fn apply_modifier(p: &mut DateTime, z: &str) -> Result<()> {
    let z_lower = z.to_lowercase();

    match z_lower.chars().next() {
        Some('a') if z_lower == "auto" => {
            auto_adjust_date(p);
            Ok(())
        }
        Some('c') if z_lower == "ceiling" => {
            p.compute_jd();
            p.clear_ymd_hms_tz();
            p.n_floor = 0;
            Ok(())
        }
        Some('f') if z_lower == "floor" => {
            p.compute_jd();
            if p.n_floor != 0 {
                p.i_jd -= p.n_floor as i64 * JD_TO_MS;
                p.n_floor = 0;
            }
            p.clear_ymd_hms_tz();
            Ok(())
        }
        Some('j') if z_lower == "julianday" => {
            if p.valid_jd && p.raw_s {
                p.raw_s = false;
                Ok(())
            } else {
                Err(InvalidModifier(format!(
                    "Invalid use of julianday modifier: {}",
                    z
                )))
            }
        }
        Some('l') if z_lower == "localtime" => {
            if !p.is_local {
                p.compute_jd();
                let timestamp = (p.i_jd - 210866760000000) / 1000;
                let offset_sec = match Local.timestamp_opt(timestamp, 0) {
                    chrono::LocalResult::Single(dt) => dt.offset().fix().local_minus_utc(),
                    _ => 0,
                };
                p.i_jd += (offset_sec as i64) * 1000;
                p.clear_ymd_hms_tz();
                p.is_local = true;
                p.is_utc = false;
            }
            Ok(())
        }
        Some('u') if z_lower == "unixepoch" => {
            if p.raw_s {
                let r = p.s * 1000.0 + 210866760000000.0;
                p.i_jd = (r + 0.5) as i64;
                p.valid_jd = true;
                p.raw_s = false;
                p.clear_ymd_hms_tz();
                Ok(())
            } else {
                Err(InvalidModifier(format!(
                    "Invalid use of unixepoch modifier: {}",
                    z
                )))
            }
        }
        Some('u') if z_lower == "utc" => {
            if !p.is_utc {
                p.compute_jd();
                let timestamp = (p.i_jd - 210866760000000) / 1000;
                let offset_sec = match Local.timestamp_opt(timestamp, 0) {
                    chrono::LocalResult::Single(dt) => dt.offset().fix().local_minus_utc(),
                    _ => 0,
                };
                p.i_jd -= (offset_sec as i64) * 1000;
                p.clear_ymd_hms_tz();
                p.is_utc = true;
                p.is_local = false;
            }
            Ok(())
        }
        Some('w') if z_lower.starts_with("weekday ") => {
            if let Ok(val) = z[8..].trim().parse::<f64>() {
                if val >= 0.0 && val < 7.0 && (val as i64 as f64) == val {
                    let n = val as i64;
                    p.compute_ymd_hms();
                    p.valid_jd = false;
                    p.compute_jd();
                    let mut z = ((p.i_jd + 129600000) / 86400000) % 7;
                    if z > n {
                        z -= 7;
                    }
                    p.i_jd += (n - z) * 86400000;
                    p.clear_ymd_hms_tz();
                    return Ok(());
                }
            }
            Err(InvalidModifier(format!("Invalid weekday: {}", z)))
        }
        Some('s') if z_lower.starts_with("start of ") => {
            p.compute_ymd();
            p.valid_hms = true;
            p.h = 0;
            p.min = 0;
            p.s = 0.0;
            p.raw_s = false;
            p.valid_jd = false;
            p.tz = 0;
            p.n_floor = 0;
            let suffix = &z_lower[9..];
            if suffix == "month" {
                p.d = 1;
                Ok(())
            } else if suffix == "year" {
                p.m = 1;
                p.d = 1;
                Ok(())
            } else if suffix == "day" {
                Ok(())
            } else {
                Err(InvalidModifier(format!("Invalid start of: {}", z)))
            }
        }
        Some('s') if z_lower == "subsec" || z_lower == "subsecond" => {
            p.use_subsec = true;
            Ok(())
        }
        Some('+') | Some('-') | Some('0'..='9') => parse_arithmetic_modifier(p, z),
        _ => Err(InvalidModifier(format!("Unknown modifier: {}", z))),
    }
}
fn parse_arithmetic_modifier(p: &mut DateTime, z: &str) -> Result<()> {
    let z = z.trim();
    let is_neg = z.starts_with('-');
    let sign = if is_neg { -1 } else { 1 };

    let mut clean_z = if z.starts_with('+') || z.starts_with('-') {
        &z[1..]
    } else {
        z
    };
    let end_date_idx = clean_z.find(' ').unwrap_or(clean_z.len());
    let date_part = &clean_z[..end_date_idx];

    // Case 1: YYYY-MM-DD Arithmetic
    if date_part.len() >= 8 && date_part.contains('-') {
        if let Some((y, m, d)) = parse_modifier_ymd(date_part) {
            p.compute_ymd_hms();
            p.valid_jd = false;

            let y_adj = y as i64;
            let m_adj = m as i64;
            let d_adj = d as i64;

            if is_neg {
                p.y = p.y.wrapping_sub(y_adj as i32);
                p.m = p.m.wrapping_sub(m_adj as i32);
            } else {
                p.y = p.y.wrapping_add(y_adj as i32);
                p.m = p.m.wrapping_add(m_adj as i32);
            }

            let m_current = p.m as i64;
            let x = if m_current > 0 {
                (m_current - 1) / 12
            } else {
                (m_current - 12) / 12
            };
            p.y = p.y.wrapping_add(x as i32);
            p.m = (m_current - x * 12) as i32;

            p.compute_floor();
            p.compute_jd();

            let day_diff = if is_neg { -d_adj } else { d_adj };
            p.i_jd = p.i_jd.wrapping_add(day_diff.wrapping_mul(JD_TO_MS));

            if end_date_idx < clean_z.len() {
                let time_part = &clean_z[end_date_idx..].trim();
                if !time_part.is_empty() {
                    let mut tx = DateTime::default();
                    if parse_hh_mm_ss(time_part, &mut tx) {
                        tx.compute_jd();
                        let ms = (tx.h as i64 * 3600000)
                            + (tx.min as i64 * 60000)
                            + (tx.s * 1000.0) as i64;
                        p.i_jd = p.i_jd.wrapping_add((sign as i64).wrapping_mul(ms));
                    }
                }
            }
            p.clear_ymd_hms_tz();
            return Ok(());
        }
    }

    // Case 2: HH:MM:SS Arithmetic
    if z.contains(':') {
        let mut tx = DateTime::default();
        let time_str = if z.starts_with('+') || z.starts_with('-') {
            &z[1..]
        } else {
            z
        };
        if parse_hh_mm_ss(time_str, &mut tx) {
            tx.compute_jd();
            let ms = (tx.h as i64 * 3600000) + (tx.min as i64 * 60000) + (tx.s * 1000.0) as i64;
            p.compute_jd();
            p.i_jd = p.i_jd.wrapping_add((sign as i64).wrapping_mul(ms));
            p.clear_ymd_hms_tz();
            return Ok(());
        }
    }

    // Case 3: NNN Units
    let parts: Vec<&str> = z.split_whitespace().collect();
    if parts.len() >= 2 {
        if let Ok(val) = parts[0].parse::<f64>() {
            let unit = parts[1].to_lowercase();
            let limit_check = |v: f64, limit: f64| v.abs() < limit;

            match unit.as_str() {
                "day" | "days" | "hour" | "hours" | "minute" | "minutes" | "second" | "seconds" => {
                    let limit = match unit.as_str() {
                        "day" | "days" => LIMIT_DAY,
                        "hour" | "hours" => LIMIT_HOUR,
                        "minute" | "minutes" => LIMIT_MINUTE,
                        "second" | "seconds" => LIMIT_SECOND,
                        _ => 0.0,
                    };
                    if !limit_check(val, limit) {
                        return Err(InvalidModifier(format!("Modifier out of range: {}", z)));
                    }

                    p.compute_jd();
                    let ms = match unit.as_str() {
                        "day" | "days" => val * 86400000.0,
                        "hour" | "hours" => val * 3600000.0,
                        "minute" | "minutes" => val * 60000.0,
                        "second" | "seconds" => val * 1000.0,
                        _ => 0.0,
                    };
                    let rounder = if ms < 0.0 { -0.5 } else { 0.5 };
                    p.i_jd = p.i_jd.wrapping_add((ms + rounder) as i64);
                    p.n_floor = 0;
                    p.clear_ymd_hms_tz();
                    return Ok(());
                }
                "month" | "months" => {
                    if !limit_check(val, LIMIT_MONTH) {
                        return Err(InvalidModifier(format!("Modifier out of range: {}", z)));
                    }
                    p.compute_ymd_hms();
                    let int_months = val as i64;
                    let frac_months = val - int_months as f64;

                    let total_months = (p.m as i64) + int_months;
                    let x = if total_months > 0 {
                        (total_months - 1) / 12
                    } else {
                        (total_months - 12) / 12
                    };
                    p.y = p.y.wrapping_add(x as i32);
                    p.m = (total_months - x * 12) as i32;

                    p.compute_floor();
                    p.valid_jd = false;
                    p.compute_jd();

                    if frac_months.abs() > f64::EPSILON {
                        let ms = frac_months * 30.0 * JD_TO_MS as f64;
                        let rounder = if ms < 0.0 { -0.5 } else { 0.5 };
                        p.i_jd = p.i_jd.wrapping_add((ms + rounder) as i64);
                    }
                    p.clear_ymd_hms_tz();
                    return Ok(());
                }
                "year" | "years" => {
                    if !limit_check(val, LIMIT_YEAR) {
                        return Err(InvalidModifier(format!("Modifier out of range: {}", z)));
                    }
                    p.compute_ymd_hms();
                    let int_years = val as i64;
                    let frac_years = val - int_years as f64;

                    p.y = p.y.wrapping_add(int_years as i32);

                    p.compute_floor();
                    p.valid_jd = false;
                    p.compute_jd();

                    if frac_years.abs() > f64::EPSILON {
                        let ms = frac_years * 365.0 * JD_TO_MS as f64;
                        let rounder = if ms < 0.0 { -0.5 } else { 0.5 };
                        p.i_jd = p.i_jd.wrapping_add((ms + rounder) as i64);
                    }
                    p.clear_ymd_hms_tz();
                    return Ok(());
                }
                _ => {}
            }
        }
    }

    Err(InvalidModifier(format!(
        "Invalid arithmetic modifier: {}",
        z
    )))
}

fn is_leap(y: i32) -> bool {
    (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0)
}

pub fn exec_date<I, E, V>(values: I) -> Value
where
    V: AsValueRef,
    E: ExactSizeIterator<Item = V>,
    I: IntoIterator<IntoIter = E, Item = V>,
{
    exec_datetime_general(values, "date")
}

pub fn exec_time<I, E, V>(values: I) -> Value
where
    V: AsValueRef,
    E: ExactSizeIterator<Item = V>,
    I: IntoIterator<IntoIter = E, Item = V>,
{
    exec_datetime_general(values, "time")
}

pub fn exec_datetime_full<I, E, V>(values: I) -> Value
where
    V: AsValueRef,
    E: ExactSizeIterator<Item = V>,
    I: IntoIterator<IntoIter = E, Item = V>,
{
    exec_datetime_general(values, "datetime")
}

pub fn exec_julianday<I, E, V>(values: I) -> Value
where
    V: AsValueRef,
    E: ExactSizeIterator<Item = V>,
    I: IntoIterator<IntoIter = E, Item = V>,
{
    exec_datetime_general(values, "julianday")
}

pub fn exec_unixepoch<I, E, V>(values: I) -> Value
where
    V: AsValueRef,
    E: ExactSizeIterator<Item = V>,
    I: IntoIterator<IntoIter = E, Item = V>,
{
    exec_datetime_general(values, "unixepoch")
}

pub fn exec_datetime_general<I, E, V>(values: I, func_type: &str) -> Value
where
    V: AsValueRef,
    E: ExactSizeIterator<Item = V>,
    I: IntoIterator<IntoIter = E, Item = V>,
{
    let mut values = values.into_iter();
    let mut p = DateTime::default();
    let mut has_modifier = false;

    if values.len() == 0 {
        set_to_current(&mut p);
    } else {
        let first = values.next().unwrap();
        match first.as_value_ref() {
            ValueRef::Text(s) => {
                if parse_date(s.as_str(), &mut p).is_err() {
                    return Value::Null;
                }
            }
            ValueRef::Integer(i) => {
                p.s = i as f64;
                p.raw_s = true;
                if p.s >= 0.0 && p.s < 5373484.5 {
                    p.i_jd = (p.s * JD_TO_MS as f64 + 0.5) as i64;
                    p.valid_jd = true;
                }
            }
            ValueRef::Float(f) => {
                p.s = f;
                p.raw_s = true;
                if p.s >= 0.0 && p.s < 5373484.5 {
                    p.i_jd = (p.s * JD_TO_MS as f64 + 0.5) as i64;
                    p.valid_jd = true;
                }
            }
            _ => return Value::Null,
        }
    }

    for val in values {
        has_modifier = true;
        if let ValueRef::Text(s) = val.as_value_ref() {
            if apply_modifier(&mut p, s.as_str()).is_err() {
                return Value::Null;
            }
        } else {
            return Value::Null;
        }
    }

    p.compute_jd();
    if p.is_error || p.i_jd < 0 || p.i_jd > MAX_JD {
        return Value::Null;
    }

    if !has_modifier && p.valid_ymd && p.d > 28 {
        p.valid_ymd = false;
    }

    match func_type {
        "julianday" => Value::Float(p.i_jd as f64 / 86400000.0),
        "unixepoch" => {
            let unix = (p.i_jd - 210866760000000) / 1000;
            if p.use_subsec {
                let ms = (p.i_jd - 210866760000000) as f64 / 1000.0;
                Value::Float(ms)
            } else {
                Value::Integer(unix)
            }
        }
        _ => {
            p.compute_ymd_hms();
            if p.is_error {
                return Value::Null;
            }

            let mut res = String::new();
            if func_type == "date" {
                write!(res, "{:04}-{:02}-{:02}", p.y, p.m, p.d).unwrap();
            } else if func_type == "time" {
                write!(res, "{:02}:{:02}", p.h, p.min).unwrap();
                if p.use_subsec {
                    write!(res, ":{:06.3}", p.s).unwrap();
                } else {
                    write!(res, ":{:02}", p.s as i32).unwrap();
                }
            } else {
                write!(
                    res,
                    "{:04}-{:02}-{:02} {:02}:{:02}",
                    p.y, p.m, p.d, p.h, p.min
                )
                .unwrap();
                if p.use_subsec {
                    write!(res, ":{:06.3}", p.s).unwrap();
                } else {
                    write!(res, ":{:02}", p.s as i32).unwrap();
                }
            }
            Value::from_text(res)
        }
    }
}

pub fn exec_timediff<I, E, V>(values: I) -> Value
where
    V: AsValueRef,
    E: ExactSizeIterator<Item = V>,
    I: IntoIterator<IntoIter = E, Item = V>,
{
    let mut values = values.into_iter();
    if values.len() < 2 {
        return Value::Null;
    }

    let mut d1 = DateTime::default();
    let mut d2 = DateTime::default();

    if let ValueRef::Text(s) = values.next().unwrap().as_value_ref() {
        if parse_date(s.as_str(), &mut d1).is_err() {
            return Value::Null;
        }
    } else {
        return Value::Null;
    }

    if let ValueRef::Text(s) = values.next().unwrap().as_value_ref() {
        if parse_date(s.as_str(), &mut d2).is_err() {
            return Value::Null;
        }
    } else {
        return Value::Null;
    }

    d1.compute_jd();
    d2.compute_jd();

    d1.compute_ymd_hms();
    d2.compute_ymd_hms();

    let sign: char;
    if d1.i_jd >= d2.i_jd {
        sign = '+';
    } else {
        sign = '-';
        std::mem::swap(&mut d1, &mut d2);
    }

    let mut y = d1.y - d2.y;
    let mut m = d1.m - d2.m;

    if m < 0 {
        y -= 1;
        m += 12;
    }

    let mut temp = d2;
    temp.y += y;
    temp.m += m;

    // Normalize months
    while temp.m > 12 {
        temp.m -= 12;
        temp.y += 1;
    }
    while temp.m < 1 {
        temp.m += 12;
        temp.y -= 1;
    }

    temp.valid_jd = false;
    temp.compute_jd();

    // Adjust if the Y/M shift overshot d1
    while temp.i_jd > d1.i_jd {
        m -= 1;
        if m < 0 {
            m = 11;
            y -= 1;
        }
        temp = d2;
        temp.y += y;
        temp.m += m;
        while temp.m > 12 {
            temp.m -= 12;
            temp.y += 1;
        }
        while temp.m < 1 {
            temp.m += 12;
            temp.y -= 1;
        }
        temp.valid_jd = false;
        temp.compute_jd();
    }

    let diff_ms = d1.i_jd - temp.i_jd;
    let days = diff_ms / 86400000;
    let rem_ms = diff_ms % 86400000;
    let hours = rem_ms / 3600000;
    let rem_ms = rem_ms % 3600000;
    let mins = rem_ms / 60000;
    let rem_ms = rem_ms % 60000;
    let secs = rem_ms as f64 / 1000.0;

    let mut res = String::new();
    write!(
        res,
        "{}{:04}-{:02}-{:02} {:02}:{:02}:{:06.3}",
        sign, y, m, days, hours, mins, secs
    )
    .unwrap();

    Value::from_text(res)
}

pub fn exec_strftime<I, E, V>(values: I) -> Value
where
    V: AsValueRef,
    E: ExactSizeIterator<Item = V>,
    I: IntoIterator<IntoIter = E, Item = V>,
{
    let mut values = values.into_iter();
    if values.len() < 1 {
        return Value::Null;
    }

    let fmt_val = values.next().unwrap();
    let fmt_str = match fmt_val.as_value_ref() {
        ValueRef::Text(s) => s.as_str(),
        _ => return Value::Null,
    };

    let mut p = DateTime::default();
    if values.len() == 0 {
        set_to_current(&mut p);
    } else {
        let init_val = values.next().unwrap();
        match init_val.as_value_ref() {
            ValueRef::Text(s) => {
                let s_str = s.as_str();
                if s_str.eq_ignore_ascii_case("now") {
                    set_to_current(&mut p);
                } else if let Ok(val) = s_str.parse::<f64>() {
                    p.s = val;
                    p.raw_s = true;
                    if p.s >= 0.0 && p.s < 5373484.5 {
                        p.i_jd = (p.s * JD_TO_MS as f64 + 0.5) as i64;
                        p.valid_jd = true;
                    }
                } else {
                    let mut temp_p = DateTime::default();
                    if parse_date(s_str, &mut temp_p).is_ok() {
                        p = temp_p;
                    } else {
                        return Value::Null;
                    }
                }
            }
            ValueRef::Integer(i) => {
                p.s = i as f64;
                p.raw_s = true;
                if p.s >= 0.0 && p.s < 5373484.5 {
                    p.i_jd = (p.s * JD_TO_MS as f64 + 0.5) as i64;
                    p.valid_jd = true;
                }
            }
            ValueRef::Float(f) => {
                p.s = f;
                p.raw_s = true;
                if p.s >= 0.0 && p.s < 5373484.5 {
                    p.i_jd = (p.s * JD_TO_MS as f64 + 0.5) as i64;
                    p.valid_jd = true;
                }
            }
            _ => return Value::Null,
        }

        for val in values {
            if let ValueRef::Text(s) = val.as_value_ref() {
                if apply_modifier(&mut p, s.as_str()).is_err() {
                    return Value::Null;
                }
            } else {
                return Value::Null;
            }
        }
    }

    p.compute_jd();
    if p.is_error {
        return Value::Null;
    }

    p.compute_ymd_hms();

    let mut res = String::new();
    let mut chars = fmt_str.chars().peekable();

    let days_after_jan1 = |curr: &DateTime| -> i64 {
        let jan1 = DateTime {
            y: curr.y,
            m: 1,
            d: 1,
            valid_ymd: true,
            ..Default::default()
        };
        let mut j1 = jan1;
        j1.compute_jd();
        let curr_norm = DateTime {
            y: curr.y,
            m: curr.m,
            d: curr.d,
            valid_ymd: true,
            ..Default::default()
        };
        let mut c1 = curr_norm;
        c1.compute_jd();
        (c1.i_jd - j1.i_jd) / JD_TO_MS
    };

    let days_after_mon = |curr: &DateTime| -> i64 { ((curr.i_jd + 43200000) / JD_TO_MS) % 7 };
    let days_after_sun = |curr: &DateTime| -> i64 { ((curr.i_jd + 129600000) / JD_TO_MS) % 7 };

    while let Some(c) = chars.next() {
        if c != '%' {
            res.push(c);
            continue;
        }

        match chars.next() {
            Some('d') => write!(res, "{:02}", p.d).unwrap(),
            Some('e') => write!(res, "{:2}", p.d).unwrap(),
            Some('F') => write!(res, "{:04}-{:02}-{:02}", p.y, p.m, p.d).unwrap(),
            Some('f') => {
                let mut s = p.s;
                if s > 59.999 {
                    s = 59.999;
                }
                write!(res, "{:06.3}", s).unwrap()
            }
            Some('g') => {
                let mut y_iso = p.clone();
                y_iso.i_jd += (3 - days_after_mon(&p)) * 86400000;
                y_iso.valid_ymd = false;
                y_iso.compute_ymd();
                write!(res, "{:02}", y_iso.y % 100).unwrap();
            }
            Some('G') => {
                let mut y_iso = p.clone();
                y_iso.i_jd += (3 - days_after_mon(&p)) * 86400000;
                y_iso.valid_ymd = false;
                y_iso.compute_ymd();
                write!(res, "{:04}", y_iso.y).unwrap();
            }
            Some('H') => write!(res, "{:02}", p.h).unwrap(),
            Some('I') => {
                let h = if p.h % 12 == 0 { 12 } else { p.h % 12 };
                write!(res, "{:02}", h).unwrap();
            }
            Some('j') => {
                write!(res, "{:03}", days_after_jan1(&p) + 1).unwrap();
            }
            Some('J') => {
                let val = p.i_jd as f64 / 86400000.0;
                if val.abs() >= 1_000_000.0 && val.abs() < 10_000_000.0 {
                    let s = format!("{:.9}", val);
                    let trimmed = s.trim_end_matches('0').trim_end_matches('.');
                    write!(res, "{}", trimmed).unwrap();
                } else {
                    write!(res, "{}", val).unwrap();
                }
            }
            Some('k') => write!(res, "{:2}", p.h).unwrap(),
            Some('l') => {
                let h = if p.h % 12 == 0 { 12 } else { p.h % 12 };
                write!(res, "{:2}", h).unwrap();
            }
            Some('m') => write!(res, "{:02}", p.m).unwrap(),
            Some('M') => write!(res, "{:02}", p.min).unwrap(),
            Some('p') => write!(res, "{}", if p.h >= 12 { "PM" } else { "AM" }).unwrap(),
            Some('P') => write!(res, "{}", if p.h >= 12 { "pm" } else { "am" }).unwrap(),
            Some('R') => write!(res, "{:02}:{:02}", p.h, p.min).unwrap(),
            Some('s') => write!(res, "{}", (p.i_jd - 210866760000000) / 1000).unwrap(),
            Some('S') => write!(res, "{:02}", p.s as i32).unwrap(),
            Some('T') => write!(res, "{:02}:{:02}:{:02}", p.h, p.min, p.s as i32).unwrap(),
            Some('u') => {
                let mut w = days_after_sun(&p);
                if w == 0 {
                    w = 7;
                }
                write!(res, "{}", w).unwrap();
            }
            Some('U') => {
                let w = (days_after_jan1(&p) - days_after_sun(&p) + 7) / 7;
                write!(res, "{:02}", w).unwrap();
            }
            Some('V') => {
                let mut temp = p.clone();
                temp.i_jd += (3 - days_after_mon(&p)) * 86400000;
                temp.valid_ymd = false;
                temp.compute_ymd();
                let w = days_after_jan1(&temp) / 7 + 1;
                write!(res, "{:02}", w).unwrap();
            }
            Some('w') => {
                write!(res, "{}", days_after_sun(&p)).unwrap();
            }
            Some('W') => {
                let w = (days_after_jan1(&p) - days_after_mon(&p) + 7) / 7;
                write!(res, "{:02}", w).unwrap();
            }
            Some('Y') => write!(res, "{:04}", p.y).unwrap(),
            Some('%') => res.push('%'),
            _ => return Value::Null,
        }
    }

    Value::from_text(res)
}
