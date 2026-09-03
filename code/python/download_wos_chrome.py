# ============================================================
# Web of Science 分任务批量导出 - Google Chrome 版
#
# 使用方法：
#
# 1. 直接运行本脚本
# 2. 使用脚本打开的“专用 Chrome”
# 3. 在 WoS 中手动筛选你要下载的年份/时间段
# 4. 停留在搜索结果页面
# 5. 回终端按 Enter
# 6. 输入本次任务名，例如：
#
#       1990
#       1998
#       2021
#       2022_H1
#       2022_H2
#
# 程序自动：
#   - 读取当前WoS结果数
#   - 创建独立任务文件夹
#   - 每批500条
#   - 独立记录download_log.csv
#   - 支持断点续传
#   - 检测WoS服务器导出超时
#
# 保存示例：
#
# D:\Python\WOS_AI\1998\
#
# ============================================================


import csv
import json
import os
import random
import re
import subprocess
import time
import urllib.request
from pathlib import Path

from playwright.sync_api import sync_playwright


# ============================================================
# 0. 基本参数
# ============================================================

# WoS 当前单次最多500条
BATCH_SIZE = 500


# 一般始终保持1
# 断点续传由download_log.csv负责
START_BATCH = 1


# None = 当前任务一直运行到最后
END_BATCH = None


# False = 正式连续下载
# True  = 只测试一个批次
DRY_RUN = False


# ============================================================
# 导出格式
# ============================================================

EXPORT_FORMAT = "Plain text file"

RECORD_CONTENT = "Full Record and Cited References"


# ============================================================
# WoS搜索结果窗口上限
# ============================================================

MAX_RESULT_WINDOW = 100_000


# ============================================================
# 总下载目录
# ============================================================

BASE_DOWNLOAD_DIR = Path(
    r"D:\Python\WOS_AI"
)

BASE_DOWNLOAD_DIR.mkdir(
    parents=True,
    exist_ok=True
)


# ============================================================
# Chrome专用Profile
# ============================================================

CHROME_PROFILE_DIR = Path(
    r"D:\WOS_AI_2026\chrome_wos_profile"
)

CHROME_PROFILE_DIR.mkdir(
    parents=True,
    exist_ok=True
)


# ============================================================
# Chrome Remote Debugging
# ============================================================

REMOTE_PORT = 9222

CDP_URL = (
    f"http://127.0.0.1:{REMOTE_PORT}"
)


# ============================================================
# 等待设置
# ============================================================

# 普通页面操作
TIMEOUT_MS = 90_000


# 最长等待下载5分钟
DOWNLOAD_TIMEOUT_MS = 300_000


# 正常批次之间暂停
MIN_SLEEP = 7
MAX_SLEEP = 12


# 每批最多尝试3次
MAX_RETRY = 3


# 失败后的等待时间
# 第一次失败后30秒
# 第二次失败后90秒
RETRY_WAIT = {
    1: 30,
    2: 90
}


# ============================================================
# 1. 查找 Google Chrome
# ============================================================

def find_chrome():

    candidates = [

        Path(
            r"C:\Program Files\Google\Chrome\Application\chrome.exe"
        ),

        Path(
            r"C:\Program Files (x86)\Google\Chrome\Application\chrome.exe"
        ),

        (
            Path(
                os.environ.get(
                    "LOCALAPPDATA",
                    ""
                )
            )
            / "Google"
            / "Chrome"
            / "Application"
            / "chrome.exe"
        )
    ]


    for path in candidates:

        if path.exists():

            return path


    raise RuntimeError(
        "没有找到 Google Chrome。\n"
        "请确认 Chrome 已正确安装。"
    )


# ============================================================
# 2. 检查 Remote Debugging
# ============================================================

def cdp_is_running():

    try:

        with urllib.request.urlopen(
            f"{CDP_URL}/json/version",
            timeout=2
        ) as response:

            return (
                response.status == 200
            )

    except Exception:

        return False


# ============================================================
# 3. 启动专用 Chrome
# ============================================================

def start_debug_chrome():

    if cdp_is_running():

        print(
            "✔ 已检测到 WoS 专用 Chrome"
        )

        return


    chrome_path = find_chrome()


    print(
        "\n正在启动 WoS 专用 Chrome..."
    )


    args = [

        str(chrome_path),

        f"--remote-debugging-port={REMOTE_PORT}",

        f"--user-data-dir={CHROME_PROFILE_DIR}",

        "--no-first-run",

        "--no-default-browser-check",

        "--start-maximized",

        "--new-window"
    ]


    subprocess.Popen(
        args,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )


    # 最多等30秒
    for _ in range(30):

        if cdp_is_running():

            print(
                "✔ WoS 专用 Chrome 已启动"
            )

            return


        time.sleep(1)


    raise RuntimeError(
        "Chrome 已启动，但无法连接 "
        f"Remote Debugging 端口 {REMOTE_PORT}。"
    )


# ============================================================
# 4. 找第一个可见元素
# ============================================================

def first_visible(locator):

    try:

        count = locator.count()

    except Exception:

        return None


    for i in range(count):

        item = locator.nth(i)


        try:

            if item.is_visible():

                return item

        except Exception:

            pass


    return None


# ============================================================
# 5. 查找 WoS 搜索结果页
# ============================================================

def find_wos_result_page(context):

    print(
        "\n正在检查 Chrome 中的标签页..."
    )


    result_pages = []


    for i, page in enumerate(
        context.pages
    ):

        try:

            print(
                f"[{i}] "
                f"{page.title()} | "
                f"{page.url}"
            )


            url = (
                page.url.lower()
            )


            if (
                "webofscience.clarivate"
                in url
                and
                "/summary/"
                in url
            ):

                result_pages.append(
                    page
                )


        except Exception:

            pass


    if not result_pages:

        return None


    # 最后一个WoS结果页
    page = result_pages[-1]


    page.bring_to_front()


    page.set_default_timeout(
        TIMEOUT_MS
    )


    return page


# ============================================================
# 6. 一直等待用户打开 WoS 结果页
# ============================================================

def wait_for_wos_result_page(
    context
):

    while True:

        page = find_wos_result_page(
            context
        )


        if page is not None:

            return page


        print(
            "\n⚠ 当前【专用 Chrome】中还没有检测到 WoS 结果页。"
        )


        print(
            "\n请一定在脚本打开的这个 Chrome 中："
        )


        print(
            "1. 进入 Web of Science"
        )


        print(
            "2. 打开你的AI检索结果"
        )


        print(
            "3. 手动筛选好年份/时间段"
        )


        print(
            "4. 停留在显示结果数量的页面"
        )


        input(
            "\n完成后回到终端按 Enter，程序重新检测..."
        )


# ============================================================
# 7. 自动读取结果总数
# ============================================================

# ============================================================
# 自动读取当前WoS结果总数
# 会等待页面真正加载完成
# ============================================================

def read_total_records(page, max_wait_seconds=60):

    print("\n正在读取当前 WoS 可见结果数...")

    deadline = time.time() + max_wait_seconds

    while time.time() < deadline:

        try:

            page.bring_to_front()

            try:
                page.wait_for_load_state(
                    "domcontentloaded",
                    timeout=5000
                )
            except Exception:
                pass

            # ------------------------------------------------
            # 优先匹配当前页面真正显示的：
            # 3,523 results from Web of Science Core Collection
            # ------------------------------------------------

            pattern = re.compile(
                r"([\d,]+)\s+results?\s+from\s+"
                r"Web\s+of\s+Science\s+Core\s+Collection",
                re.I
            )

            candidates = page.get_by_text(
                pattern
            )

            visible_values = []

            for i in range(candidates.count()):

                item = candidates.nth(i)

                try:

                    if not item.is_visible():
                        continue

                    txt = item.inner_text().strip()

                    match = pattern.search(txt)

                    if match:

                        n = int(
                            match.group(1).replace(",", "")
                        )

                        visible_values.append(n)

                except Exception:
                    pass


            # ------------------------------------------------
            # 如果找到了可见结果，直接使用
            # ------------------------------------------------

            if visible_values:

                # 去重
                visible_values = list(
                    dict.fromkeys(visible_values)
                )

                print(
                    "页面检测到的可见结果数：",
                    visible_values
                )

                # 正常情况下应该只有一个
                if len(visible_values) == 1:

                    n = visible_values[0]

                    print(
                        f"✔ 当前 WoS 结果数：{n:,}"
                    )

                    return n

                # 如果有多个，取当前页面中最大的那个，
                # 同时给出提示
                n = max(visible_values)

                print(
                    f"⚠ 检测到多个可见结果数，"
                    f"暂采用：{n:,}"
                )

                return n


        except Exception as e:

            print(
                "读取结果数时暂时失败：",
                e
            )


        print(
            "页面尚未完全稳定，2秒后重新检测..."
        )

        time.sleep(2)


    return None


    # =========================================================
    # 60秒还没找到
    # =========================================================

    print(
        "\n⚠ 60秒内没有自动识别到结果数。"
    )

    if last_text:

        print(
            "\n当前页面文字前1000字符：\n"
        )

        print(
            last_text[:1000]
        )


    return None


# ============================================================
# 8. 获取当前 WoS Summary ID
# ============================================================

def get_summary_id(page):

    url = page.url


    match = re.search(
        r"/summary/([^/]+)/",
        url,
        re.I
    )


    if match:

        return match.group(1)


    return ""


# ============================================================
# 9. 清理用户输入的任务名
# ============================================================

def sanitize_task_name(
    task_name
):

    task_name = (
        task_name.strip()
    )


    # Windows文件夹非法字符替换成_
    task_name = re.sub(
        r'[<>:"/\\|?*]',
        "_",
        task_name
    )


    task_name = (
        task_name.strip(
            ". "
        )
    )


    if not task_name:

        raise RuntimeError(
            "任务名不能为空。"
        )


    return task_name


# ============================================================
# 10. 创建独立任务目录
# ============================================================

def setup_task_paths(
    task_name
):

    download_dir = (
        BASE_DOWNLOAD_DIR
        / task_name
    )


    download_dir.mkdir(
        parents=True,
        exist_ok=True
    )


    log_file = (
        download_dir
        / "download_log.csv"
    )


    error_dir = (
        download_dir
        / "errors"
    )


    error_dir.mkdir(
        parents=True,
        exist_ok=True
    )


    task_info_file = (
        download_dir
        / "task_info.json"
    )


    return (
        download_dir,
        log_file,
        error_dir,
        task_info_file
    )


# ============================================================
# 11. 保存/检查任务信息
# ============================================================

def check_task_info(
    task_info_file,
    task_name,
    total_records,
    page
):

    current_info = {

        "task_name":
        task_name,

        "total_records":
        total_records,

        "batch_size":
        BATCH_SIZE,

        "record_content":
        RECORD_CONTENT,

        "summary_id":
        get_summary_id(
            page
        ),

        "url":
        page.url
    }


    # --------------------------------------------------------
    # 第一次运行
    # --------------------------------------------------------

    if not task_info_file.exists():

        with open(
            task_info_file,
            "w",
            encoding="utf-8"
        ) as f:

            json.dump(
                current_info,
                f,
                ensure_ascii=False,
                indent=2
            )


        print(
            "✔ 已创建任务信息文件"
        )


        return


    # --------------------------------------------------------
    # 已有任务
    # --------------------------------------------------------

    try:

        with open(
            task_info_file,
            "r",
            encoding="utf-8"
        ) as f:

            old_info = json.load(f)


    except Exception:

        return


    old_total = old_info.get(
        "total_records"
    )


    # --------------------------------------------------------
    # 结果数发生变化时提醒
    # --------------------------------------------------------

    if (
        old_total is not None
        and
        old_total != total_records
    ):

        print(
            "\n⚠ 当前任务的WoS结果数发生变化！"
        )


        print(
            f"原记录数："
            f"{old_total:,}"
        )


        print(
            f"现在记录数："
            f"{total_records:,}"
        )


        answer = input(
            "\n如果确认仍然是同一个检索任务，"
            "输入 y 继续；其他键停止："
        )


        if answer.lower() != "y":

            raise RuntimeError(
                "用户停止运行。"
            )


# ============================================================
# 12. 初始化日志
# ============================================================

def init_log(
    log_file
):

    if log_file.exists():

        return


    with open(
        log_file,
        "w",
        newline="",
        encoding="utf-8-sig"
    ) as f:

        writer = csv.writer(f)


        writer.writerow([

            "task",

            "batch",

            "start_record",

            "end_record",

            "status",

            "file",

            "message",

            "time"
        ])


# ============================================================
# 13. 写日志
# ============================================================

def write_log(
    log_file,
    task_name,
    batch,
    start_record,
    end_record,
    status,
    file_name="",
    message=""
):

    with open(
        log_file,
        "a",
        newline="",
        encoding="utf-8-sig"
    ) as f:

        writer = csv.writer(f)


        writer.writerow([

            task_name,

            batch,

            start_record,

            end_record,

            status,

            file_name,

            message,

            time.strftime(
                "%Y-%m-%d %H:%M:%S"
            )
        ])


# ============================================================
# 14. 获取已经成功的批次
# ============================================================

def get_completed_batches(
    log_file
):

    completed = set()


    if not log_file.exists():

        return completed


    with open(
        log_file,
        "r",
        encoding="utf-8-sig"
    ) as f:

        reader = (
            csv.DictReader(f)
        )


        for row in reader:

            if (
                row.get(
                    "status"
                )
                == "success"
            ):

                try:

                    completed.add(
                        int(
                            row["batch"]
                        )
                    )

                except Exception:

                    pass


    return completed


# ============================================================
# 15. 获取 Export 对话框
# ============================================================

def get_export_dialog(page):

    dialogs = (
        page.locator(
            '[role="dialog"]'
        )
    )


    for i in reversed(
        range(
            dialogs.count()
        )
    ):

        dialog = (
            dialogs.nth(i)
        )


        try:

            if dialog.is_visible():

                return dialog

        except Exception:

            pass


    # 没有role=dialog则使用body兜底
    return page.locator(
        "body"
    )


# ============================================================
# 16. 关闭残留 Export 弹窗
# ============================================================

def close_export_dialog(page):

    try:

        page.keyboard.press(
            "Escape"
        )


        time.sleep(
            0.7
        )

    except Exception:

        pass


# ============================================================
# 点击 WoS 结果页真正的 Export 下拉按钮
# 专门排除 Export Refine 等错误按钮
# ============================================================

def click_main_export(page):

    print("正在寻找真正的结果页 Export 按钮...")

    page.bring_to_front()

    # --------------------------------------------------------
    # 关闭可能残留的旧弹窗
    # --------------------------------------------------------
    try:
        page.keyboard.press("Escape")
        time.sleep(0.5)
    except Exception:
        pass

    # --------------------------------------------------------
    # 等页面稳定
    # --------------------------------------------------------
    try:
        page.wait_for_load_state(
            "domcontentloaded",
            timeout=10000
        )
    except Exception:
        pass

    time.sleep(1)


    # ========================================================
    # 查找所有可见 button
    # ========================================================

    buttons = page.locator("button")

    candidates = []

    print("\n当前页面可见的 Export 类按钮：")

    for i in range(buttons.count()):

        btn = buttons.nth(i)

        try:

            if not btn.is_visible():
                continue

            text = btn.inner_text().strip()

            # 合并多余空格/换行
            normalized = re.sub(
                r"\s+",
                " ",
                text
            ).strip()

            if "export" in normalized.lower():

                print(
                    f"  [{i}] {repr(normalized)}"
                )

                # ------------------------------------------------
                # 真正的主 Export 通常是：
                #
                # Export
                #
                # 或：
                #
                # Export expand_more
                #
                # Material icon 的文字可能被 inner_text() 读出来
                # ------------------------------------------------

                lower_text = normalized.lower()

                if (
                    lower_text == "export"
                    or
                    lower_text == "export expand_more"
                    or
                    (
                        lower_text.startswith("export")
                        and
                        "expand_more" in lower_text
                    )
                ):

                    # 排除明显不是主导出的按钮
                    if (
                        "refine" not in lower_text
                        and
                        "citation" not in lower_text
                    ):

                        candidates.append(btn)

        except Exception:
            pass


    if not candidates:

        raise RuntimeError(
            "没有找到真正的 WoS Export 下拉按钮。\n"
            "请确认当前停留在搜索结果页面。"
        )


    # ========================================================
    # 优先选含 expand_more 的
    # ========================================================

    export_button = None

    for btn in candidates:

        try:

            text = re.sub(
                r"\s+",
                " ",
                btn.inner_text().strip()
            ).lower()

            if "expand_more" in text:

                export_button = btn
                break

        except Exception:
            pass


    # 没有 expand_more 才使用普通 Export
    if export_button is None:
        export_button = candidates[0]


    try:

        chosen_text = re.sub(
            r"\s+",
            " ",
            export_button.inner_text().strip()
        )

        print(
            f"✔ 已锁定真正的 Export 按钮："
            f"{repr(chosen_text)}"
        )

    except Exception:
        pass


    # ========================================================
    # 方法1：正常点击
    # ========================================================

    try:

        export_button.scroll_into_view_if_needed()

    except Exception:
        pass


    try:

        export_button.click(
            timeout=8000
        )

        time.sleep(1)

    except Exception as e:

        print(
            "普通点击失败，尝试 force click：",
            e
        )

        try:

            export_button.click(
                force=True,
                timeout=5000
            )

            time.sleep(1)

        except Exception as e2:

            print(
                "force click失败，尝试 JavaScript：",
                e2
            )

            export_button.evaluate(
                "(el) => el.click()"
            )

            time.sleep(1)


    # ========================================================
    # 关键：
    # 不再“点完就假装成功”
    # 必须确认 Export 菜单真的打开
    # ========================================================

    menu_patterns = [

        "Plain text file",
        "Excel",
        "EndNote",
        "Tab delimited file"
    ]


    deadline = time.time() + 10

    while time.time() < deadline:

        for menu_text in menu_patterns:

            try:

                loc = page.get_by_text(
                    re.compile(
                        re.escape(menu_text),
                        re.I
                    )
                )

                for i in range(loc.count()):

                    item = loc.nth(i)

                    if item.is_visible():

                        print(
                            "✔ Export 下拉菜单已经真正打开"
                        )

                        return

            except Exception:
                pass

        time.sleep(0.5)


    # ========================================================
    # 第一次点击没有打开菜单，再点击一次
    # ========================================================

    print(
        "⚠ 第一次点击后未检测到导出菜单，重新点击..."
    )

    try:

        export_button.click(
            force=True,
            timeout=5000
        )

    except Exception:

        export_button.evaluate(
            "(el) => el.click()"
        )


    time.sleep(1)


    # 再确认一次
    deadline = time.time() + 8

    while time.time() < deadline:

        try:

            plain = page.get_by_text(
                re.compile(
                    r"Plain\s+text\s+file",
                    re.I
                )
            )

            for i in range(plain.count()):

                if plain.nth(i).is_visible():

                    print(
                        "✔ Export 下拉菜单已经真正打开"
                    )

                    return

        except Exception:
            pass

        time.sleep(0.5)


    raise RuntimeError(
        "已经找到真正的 Export 按钮，"
        "但点击后导出菜单仍未打开。"
    )

# ============================================================
# 选择 Plain text file
# 增强版：等待菜单真正出现 + 多种定位方式
# ============================================================

def select_export_format(page):

    print("正在等待导出格式菜单出现...")

    target_text = "Plain text file"

    # 最多等待15秒
    deadline = time.time() + 15

    while time.time() < deadline:

        # ----------------------------------------------------
        # 方法1：精确文字查找
        # ----------------------------------------------------
        try:

            locators = page.get_by_text(
                re.compile(
                    r"^Plain\s+text\s+file$",
                    re.I
                )
            )

            for i in range(locators.count()):

                item = locators.nth(i)

                try:

                    if item.is_visible():

                        item.scroll_into_view_if_needed()

                        try:
                            item.click(timeout=5000)

                        except Exception:
                            try:
                                item.click(
                                    force=True,
                                    timeout=5000
                                )
                            except Exception:
                                item.evaluate(
                                    "(el) => el.click()"
                                )

                        print(
                            "✔ 已选择 Plain text file"
                        )

                        time.sleep(1)

                        return

                except Exception:
                    pass

        except Exception:
            pass


        # ----------------------------------------------------
        # 方法2：menuitem
        # ----------------------------------------------------
        try:

            items = page.locator(
                '[role="menuitem"]'
            )

            for i in range(items.count()):

                item = items.nth(i)

                try:

                    if not item.is_visible():
                        continue

                    txt = item.inner_text().strip()

                    if (
                        "plain text" in txt.lower()
                    ):

                        item.click(
                            timeout=5000
                        )

                        print(
                            "✔ 已选择 Plain text file"
                        )

                        time.sleep(1)

                        return

                except Exception:
                    pass

        except Exception:
            pass


        # ----------------------------------------------------
        # 方法3：
        # WoS有时会把菜单项放进button/a/div/span
        # ----------------------------------------------------
        try:

            candidates = page.locator(
                'button, a, div, span'
            )

            for i in range(candidates.count()):

                item = candidates.nth(i)

                try:

                    if not item.is_visible():
                        continue

                    txt = item.inner_text().strip()

                    # 必须基本精确匹配，避免误点说明文字
                    if (
                        txt.lower()
                        == "plain text file"
                    ):

                        try:

                            item.click(
                                timeout=3000
                            )

                        except Exception:

                            try:

                                item.click(
                                    force=True,
                                    timeout=3000
                                )

                            except Exception:

                                item.evaluate(
                                    "(el) => el.click()"
                                )

                        print(
                            "✔ 已选择 Plain text file"
                        )

                        time.sleep(1)

                        return

                except Exception:
                    pass

        except Exception:
            pass


        print(
            "  Plain text file 尚未出现，"
            "0.5秒后继续检测..."
        )

        time.sleep(0.5)


    # ========================================================
    # 15秒仍然没找到：打印当前可见菜单文字
    # ========================================================

    print(
        "\n⚠ 没找到 Plain text file。"
    )

    print(
        "当前页面中与 Export / File 相关的可见文字："
    )

    try:

        elements = page.locator(
            'button, a, [role="menuitem"], [role="option"]'
        )

        seen = set()

        for i in range(elements.count()):

            item = elements.nth(i)

            try:

                if not item.is_visible():
                    continue

                txt = (
                    item.inner_text()
                    .strip()
                )

                if not txt:
                    continue

                txt_lower = txt.lower()

                if (
                    "export" in txt_lower
                    or
                    "file" in txt_lower
                    or
                    "text" in txt_lower
                ):

                    if txt not in seen:

                        print(
                            "  -",
                            repr(txt)
                        )

                        seen.add(txt)

            except Exception:
                pass

    except Exception:
        pass


    raise RuntimeError(
        "等待15秒后仍找不到 "
        "Plain text file 导出选项。"
    )


# ============================================================
# 19. 填写 Records from / to
# ============================================================

def fill_record_range(
    page,
    start_record,
    end_record
):

    record_count = (
        end_record
        - start_record
        + 1
    )


    if record_count > 500:

        raise RuntimeError(
            "单次导出不能超过500条。"
        )


    if end_record > MAX_RESULT_WINDOW:

        raise RuntimeError(
            "当前记录序号超过100,000。"
        )


    dialog = (
        get_export_dialog(
            page
        )
    )


    # --------------------------------------------------------
    # 选择 Records from
    # --------------------------------------------------------

    try:

        radio = (
            dialog.get_by_role(
                "radio",
                name=re.compile(
                    "Records from",
                    re.I
                )
            )
        )


        radio_item = (
            first_visible(
                radio
            )
        )


        if radio_item is not None:

            radio_item.check()


    except Exception:

        pass


    # --------------------------------------------------------
    # 找输入框
    # --------------------------------------------------------

    inputs = (
        dialog.locator(
            "input:visible"
        )
    )


    usable_inputs = []


    for i in range(
        inputs.count()
    ):

        inp = (
            inputs.nth(i)
        )


        try:

            input_type = (

                inp.get_attribute(
                    "type"
                )

                or "text"

            ).lower()


            if input_type not in [

                "radio",

                "checkbox",

                "hidden"
            ]:

                usable_inputs.append(
                    inp
                )


        except Exception:

            pass


    if len(
        usable_inputs
    ) < 2:

        raise RuntimeError(
            "无法找到 Records from/to 输入框。"
        )


    from_box = (
        usable_inputs[0]
    )


    to_box = (
        usable_inputs[1]
    )


    from_box.fill(
        str(
            start_record
        )
    )


    to_box.fill(
        str(
            end_record
        )
    )


    print(
        f"✔ 已填写记录范围："
        f"{start_record}-{end_record}"
    )


# ============================================================
# 20. 设置 Record Content
# ============================================================

def set_record_content(page):

    dialog = (
        get_export_dialog(
            page
        )
    )


    # --------------------------------------------------------
    # 原生select
    # --------------------------------------------------------

    selects = (
        dialog.locator(
            "select:visible"
        )
    )


    for i in range(
        selects.count()
    ):

        sel = (
            selects.nth(i)
        )


        try:

            options = (
                sel.locator(
                    "option"
                )
            )


            for j in range(
                options.count()
            ):

                label = (
                    options
                    .nth(j)
                    .inner_text()
                    .strip()
                )


                if (
                    label.lower()
                    ==
                    RECORD_CONTENT.lower()
                ):

                    sel.select_option(
                        label=label
                    )


                    print(
                        "✔ Record Content = "
                        + RECORD_CONTENT
                    )


                    return


        except Exception:

            pass


    # --------------------------------------------------------
    # WoS自定义combobox
    # --------------------------------------------------------

    combos = (
        dialog.get_by_role(
            "combobox"
        )
    )


    for i in range(
        combos.count()
    ):

        combo = (
            combos.nth(i)
        )


        try:

            if not combo.is_visible():

                continue


            current = ""


            try:

                current = (
                    combo.inner_text()
                )

            except Exception:

                pass


            if (
                RECORD_CONTENT.lower()
                in current.lower()
            ):

                print(
                    "✔ 当前 Record Content 已是："
                    + RECORD_CONTENT
                )


                return


            combo.click()


            time.sleep(
                0.5
            )


            option = (
                page.get_by_text(
                    re.compile(
                        "^"
                        + re.escape(
                            RECORD_CONTENT
                        )
                        + "$",
                        re.I
                    )
                )
            )


            item = (
                first_visible(
                    option
                )
            )


            if item is not None:

                item.click()


                print(
                    "✔ Record Content = "
                    + RECORD_CONTENT
                )


                return


        except Exception:

            pass


    # --------------------------------------------------------
    # 最后检查弹窗文字
    # --------------------------------------------------------

    try:

        text = (
            dialog.inner_text()
        )


        if (
            RECORD_CONTENT.lower()
            in text.lower()
        ):

            print(
                "✔ 当前 Record Content 已正确"
            )


            return


    except Exception:

        pass


    raise RuntimeError(
        "无法设置 Record Content。"
    )


# ============================================================
# 21. 点击最终紫色 Export
# ============================================================

def click_final_export(page):

    print(
        "正在点击弹窗中的最终 Export..."
    )


    buttons = (
        page.locator(
            'button:has-text("Export")'
        )
    )


    visible_buttons = []


    for i in range(
        buttons.count()
    ):

        button = (
            buttons.nth(i)
        )


        try:

            if button.is_visible():

                text = (
                    button.inner_text()
                    .strip()
                )


                if (
                    text.lower()
                    == "export"
                ):

                    visible_buttons.append(
                        button
                    )


        except Exception:

            pass


    if not visible_buttons:

        raise RuntimeError(
            "没有找到弹窗中的最终 Export 按钮。"
        )


    # 一般最后一个可见Export就是弹窗中的紫色按钮
    final_button = (
        visible_buttons[-1]
    )


    try:

        final_button.scroll_into_view_if_needed()

    except Exception:

        pass


    time.sleep(
        0.3
    )


    # --------------------------------------------------------
    # 方法1：正常点击
    # --------------------------------------------------------

    try:

        final_button.click(
            timeout=5000
        )


        print(
            "✔ 已点击最终紫色 Export"
        )


        return


    except Exception as e:

        print(
            "普通点击失败，尝试强制点击：",
            e
        )


    # --------------------------------------------------------
    # 方法2：force click
    # --------------------------------------------------------

    try:

        final_button.click(
            force=True,
            timeout=5000
        )


        print(
            "✔ 已强制点击最终紫色 Export"
        )


        return


    except Exception as e:

        print(
            "强制点击失败，尝试JavaScript点击：",
            e
        )


    # --------------------------------------------------------
    # 方法3：JavaScript
    # --------------------------------------------------------

    try:

        final_button.evaluate(
            "(el) => el.click()"
        )


        print(
            "✔ 已使用JavaScript点击最终紫色 Export"
        )


        return


    except Exception as e:

        raise RuntimeError(
            f"最终 Export 按钮点击失败：{e}"
        )


# ============================================================
# 22. 检测 WoS 导出失败提示
# ============================================================

def has_export_timeout_error(page):

    patterns = [

        "The export process timed out",

        "Please try again later"
    ]


    for pattern in patterns:

        try:

            locator = (
                page.get_by_text(
                    re.compile(
                        re.escape(
                            pattern
                        ),
                        re.I
                    )
                )
            )


            item = (
                first_visible(
                    locator
                )
            )


            if item is not None:

                return True


        except Exception:

            pass


    return False


# ============================================================
# 23. 点击Export并等待下载 / WoS报错
# ============================================================

def wait_for_download_or_error(page):

    download_holder = {}


    def on_download(download):

        download_holder[
            "download"
        ] = download


    # --------------------------------------------------------
    # 注册一次download事件
    # --------------------------------------------------------

    page.once(
        "download",
        on_download
    )


    # --------------------------------------------------------
    # 点击最终Export
    # --------------------------------------------------------

    click_final_export(
        page
    )


    deadline = (

        time.time()

        + (
            DOWNLOAD_TIMEOUT_MS
            / 1000
        )
    )


    while (
        time.time()
        < deadline
    ):

        # ----------------------------------------------------
        # 下载已经开始
        # ----------------------------------------------------

        if (
            "download"
            in download_holder
        ):

            return download_holder[
                "download"
            ]


        # ----------------------------------------------------
        # WoS服务器自己报错
        # ----------------------------------------------------

        if has_export_timeout_error(
            page
        ):

            try:

                page.remove_listener(
                    "download",
                    on_download
                )

            except Exception:

                pass


            raise RuntimeError(

                "WoS服务器返回："
                "The export process timed out. "
                "Please try again later."
            )


        time.sleep(
            1
        )


    # --------------------------------------------------------
    # Python自身等待超时
    # --------------------------------------------------------

    try:

        page.remove_listener(
            "download",
            on_download
        )

    except Exception:

        pass


    raise RuntimeError(

        f"等待下载超过 "
        f"{DOWNLOAD_TIMEOUT_MS / 1000:.0f} 秒，"
        "仍未收到下载事件。"
    )


# ============================================================
# 24. 检测人机验证
# ============================================================

def requires_human_verification(page):

    try:

        text = (
            page.locator(
                "body"
            )
            .inner_text(
                timeout=5000
            )
            .lower()
        )


    except Exception:

        return False


    keywords = [

        "captcha",

        "verify you are human",

        "are you a robot",

        "unusual traffic",

        "验证码",

        "人机验证"
    ]


    return any(
        keyword in text
        for keyword in keywords
    )


# ============================================================
# 25. 下载一个批次
# ============================================================

def download_batch(
    page,
    task_name,
    download_dir,
    batch_number,
    start_record,
    end_record
):

    print(
        "\n"
        + "=" * 60
    )


    print(
        f"[{task_name}] "
        f"批次 {batch_number}: "
        f"{start_record}-{end_record}"
    )


    print(
        "=" * 60
    )


    page.bring_to_front()


    # --------------------------------------------------------
    # 1. 结果页 Export
    # --------------------------------------------------------

    click_main_export(
        page
    )


    # --------------------------------------------------------
    # 2. Plain text
    # --------------------------------------------------------

    select_export_format(
        page
    )


    # --------------------------------------------------------
    # 3. Records from / to
    # --------------------------------------------------------

    fill_record_range(
        page,
        start_record,
        end_record
    )


    # --------------------------------------------------------
    # 4. Full Record and Cited References
    # --------------------------------------------------------

    set_record_content(
        page
    )


    time.sleep(
        0.5
    )


    # --------------------------------------------------------
    # 5. 等下载或检测WoS报错
    # --------------------------------------------------------

    download = (
        wait_for_download_or_error(
            page
        )
    )


    # --------------------------------------------------------
    # 文件扩展名
    # --------------------------------------------------------

    suggested_name = (
        download.suggested_filename
    )


    suffix = (
        Path(
            suggested_name
        ).suffix
    )


    if not suffix:

        suffix = ".txt"


    # --------------------------------------------------------
    # 文件名
    #
    # 1998_000001_000500.txt
    # --------------------------------------------------------

    output_name = (

        f"{task_name}_"

        f"{start_record:06d}_"

        f"{end_record:06d}"

        f"{suffix}"
    )


    output_path = (
        download_dir
        / output_name
    )


    # --------------------------------------------------------
    # 保存
    # --------------------------------------------------------

    download.save_as(
        str(
            output_path
        )
    )


    # --------------------------------------------------------
    # 基本完整性检查
    # --------------------------------------------------------

    if not output_path.exists():

        raise RuntimeError(
            "文件下载完成但目标文件不存在。"
        )


    if (
        output_path.stat().st_size
        < 1000
    ):

        raise RuntimeError(
            "下载文件小于1KB，可能导出失败。"
        )


    print(
        f"✔ 已保存："
        f"{output_path}"
    )


    time.sleep(
        1
    )


    close_export_dialog(
        page
    )


    print(
        "✔ 已准备进入下一批"
    )


    return output_name


# ============================================================
# 26. 主程序
# ============================================================

def main():

    # --------------------------------------------------------
    # 启动/连接Chrome
    # --------------------------------------------------------

    start_debug_chrome()


    with sync_playwright() as p:

        print(
            "\n正在连接 WoS 专用 Chrome..."
        )


        browser = (
            p.chromium
            .connect_over_cdp(
                CDP_URL
            )
        )


        if not browser.contexts:

            raise RuntimeError(
                "Chrome已连接，"
                "但没有找到BrowserContext。"
            )


        context = (
            browser.contexts[0]
        )


        # ====================================================
        # 一直等到找到WoS结果页
        # ====================================================

        page = (
            wait_for_wos_result_page(
                context
            )
        )


        print(
            "\n✔ 已找到 WoS 搜索结果页面"
        )


        print(
            "当前URL：",
            page.url
        )


        # ====================================================
        # 自动读取结果数
        # ====================================================

        total_records = (
            read_total_records(
                page
            )
        )


        if total_records is None:

            raise RuntimeError(
                "无法读取当前WoS结果总数。"
            )


        print(
            f"\n✔ 当前WoS结果数："
            f"{total_records:,}"
        )


        # ====================================================
        # 超过100000停止
        # ====================================================

        if (
            total_records
            > MAX_RESULT_WINDOW
        ):

            raise RuntimeError(

                f"\n当前检索结果有 "
                f"{total_records:,} 条，"
                "超过100,000条。\n"

                "请在WoS中继续拆分当前检索结果，"
                "再重新运行。"
            )


        # ====================================================
        # 用户输入任务名称
        # ====================================================

        print(
            "\n请输入本次下载任务名称。"
        )


        print(
            "建议直接输入年份，例如："
        )


        print(
            "1990"
        )


        print(
            "1998"
        )


        print(
            "2021"
        )


        print(
            "\n如果某一年需要拆分，可以写："
        )


        print(
            "2022_H1"
        )


        print(
            "2022_H2"
        )


        task_name = sanitize_task_name(

            input(
                "\n任务名称："
            )
        )


        # ====================================================
        # 创建独立路径
        # ====================================================

        (
            download_dir,
            log_file,
            error_dir,
            task_info_file
        ) = setup_task_paths(
            task_name
        )


        # ====================================================
        # 检查任务信息
        # ====================================================

        check_task_info(
            task_info_file,
            task_name,
            total_records,
            page
        )


        # ====================================================
        # 日志
        # ====================================================

        init_log(
            log_file
        )


        completed_batches = (
            get_completed_batches(
                log_file
            )
        )


        # ====================================================
        # 总批数
        # ====================================================

        total_batches = (

            total_records

            + BATCH_SIZE

            - 1

        ) // BATCH_SIZE


        if END_BATCH is None:

            actual_end_batch = (
                total_batches
            )

        else:

            actual_end_batch = min(
                END_BATCH,
                total_batches
            )


        # ====================================================
        # 最终确认
        # ====================================================

        print(
            "\n"
            + "=" * 60
        )


        print(
            f"任务名称："
            f"{task_name}"
        )


        print(
            f"WoS结果数："
            f"{total_records:,}"
        )


        print(
            f"每批："
            f"{BATCH_SIZE}"
        )


        print(
            f"总批数："
            f"{total_batches}"
        )


        print(
            f"已成功批次："
            f"{len(completed_batches)}"
        )


        print(
            f"开始批次："
            f"{START_BATCH}"
        )


        print(
            f"结束批次："
            f"{actual_end_batch}"
        )


        print(
            f"保存目录："
            f"{download_dir}"
        )


        print(
            "=" * 60
        )


        input(
            "\n确认WoS筛选结果和任务名称正确，"
            "按 Enter 开始..."
        )


        # ====================================================
        # 批量下载
        # ====================================================

        for batch_number in range(
            START_BATCH,
            actual_end_batch + 1
        ):


            # ------------------------------------------------
            # 已成功批次跳过
            # ------------------------------------------------

            if (
                batch_number
                in completed_batches
            ):

                print(
                    f"○ [{task_name}] "
                    f"批次 {batch_number} "
                    f"已完成，跳过"
                )


                continue


            # ------------------------------------------------
            # 当前批次范围
            # ------------------------------------------------

            start_record = (

                (batch_number - 1)

                * BATCH_SIZE

                + 1
            )


            end_record = min(

                batch_number
                * BATCH_SIZE,

                total_records
            )


            success = False


            # =================================================
            # 重试
            # =================================================

            for attempt in range(
                1,
                MAX_RETRY + 1
            ):


                try:

                    # -----------------------------------------
                    # 人机验证
                    # -----------------------------------------

                    if (
                        requires_human_verification(
                            page
                        )
                    ):

                        input(
                            "\n检测到人机验证。"
                            "请在Chrome中人工完成，"
                            "然后回终端按 Enter..."
                        )


                    # -----------------------------------------
                    # 下载
                    # -----------------------------------------

                    file_name = (
                        download_batch(

                            page,

                            task_name,

                            download_dir,

                            batch_number,

                            start_record,

                            end_record
                        )
                    )


                    # -----------------------------------------
                    # 写成功日志
                    # -----------------------------------------

                    write_log(

                        log_file,

                        task_name,

                        batch_number,

                        start_record,

                        end_record,

                        "success",

                        file_name,

                        ""
                    )


                    success = True


                    break


                except Exception as e:

                    message = (
                        str(e)
                    )


                    print(
                        f"\n✖ "
                        f"[{task_name}] "
                        f"批次 {batch_number} "
                        f"第 {attempt} 次失败：\n"
                        f"{message}"
                    )


                    # -----------------------------------------
                    # 截图
                    # -----------------------------------------

                    screenshot_path = (

                        error_dir

                        / (
                            f"batch_"
                            f"{batch_number}_"
                            f"attempt_"
                            f"{attempt}.png"
                        )
                    )


                    try:

                        page.screenshot(

                            path=str(
                                screenshot_path
                            ),

                            full_page=False
                        )


                        print(
                            "错误截图：",
                            screenshot_path
                        )


                    except Exception:

                        pass


                    # -----------------------------------------
                    # 关闭残留导出弹窗
                    # -----------------------------------------

                    close_export_dialog(
                        page
                    )


                    # -----------------------------------------
                    # 重试等待
                    # -----------------------------------------

                    if (
                        attempt
                        < MAX_RETRY
                    ):

                        wait_time = (
                            RETRY_WAIT.get(
                                attempt,
                                90
                            )
                        )


                        print(
                            f"等待 "
                            f"{wait_time} 秒后重试..."
                        )


                        time.sleep(
                            wait_time
                        )


            # =================================================
            # 最终失败
            # =================================================

            if not success:

                write_log(

                    log_file,

                    task_name,

                    batch_number,

                    start_record,

                    end_record,

                    "failed",

                    "",

                    "连续3次失败"
                )


                answer = input(

                    "\n当前批次连续3次失败。\n"
                    "输入 c 跳过这一批继续；"
                    "其他任意键停止："
                )


                if (
                    answer.lower()
                    != "c"
                ):

                    break


            # =================================================
            # 测试模式
            # =================================================

            if DRY_RUN:

                print(
                    "\nDRY_RUN=True，"
                    "当前仅测试一个批次。"
                )


                break


            # =================================================
            # 批次之间暂停
            # =================================================

            sleep_seconds = (
                random.uniform(
                    MIN_SLEEP,
                    MAX_SLEEP
                )
            )


            print(
                f"\n暂停 "
                f"{sleep_seconds:.1f} 秒..."
            )


            time.sleep(
                sleep_seconds
            )


        # ====================================================
        # 当前任务结束
        # ====================================================

        print(
            "\n"
            + "=" * 60
        )


        print(
            f"任务 {task_name} 结束"
        )


        print(
            f"下载目录："
            f"{download_dir}"
        )


        print(
            f"日志："
            f"{log_file}"
        )


        print(
            "=" * 60
        )


# ============================================================
# 启动
# ============================================================

if __name__ == "__main__":

    main()
