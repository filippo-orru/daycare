var toggleSidebarButton = null;
var sideBar = null;

var exampleUserString = {
    "_id": "5c632a5b3ee0872e8571d9d4",
    "username": "fefe",
    "password": "123456",
    "settings": {
        "fullscreen": true
    },
    "goals": {
        "führerschein": {
            "description": "führerschein fertig machen",
            "deadline": "1999-12-31T23:00:00.000Z"
        },
        "healthy life": {
            "description": "gutes leben führen"
        }
    },
    "activities": {
        "raus": {
            "description": "raus gegangen",
            "category": "balance",
            "prodlevel": "3",
            "time": {
                "pre": "5",
                "start": "",
                "end": "",
                "post": "5"
            },
            "autoedit": false,
            "goals": ["healthy life"]
        }
    },
    "attributes": {
        "alcohol": {
            "short": "a",
            "description": "drank alcohol that day"
        }
    },
    "days": {
        "12.02.2019": {
            "Attributes": ["X", "A"],
            "Tasks": [{
                "name": "raus",
                "state": "completed",
                "time": {
                    "start": "12:45",
                    "end": "13:10",
                    "pre": "10",
                    "post": "0"
                }
            }, {
                "name": "ethik",
                "state": "todo",
                "time": {
                    "start": "00:00",
                    "end": "23:55",
                    "pre": "0",
                    "post": "0"
                }
            }],
            "description": "day was very good"
        }
    }
}

function findItems() {
    toggleSidebarButton = null;
    sideBar = $('.sidebar');
}

function attachEvenListeners() {
    // findItems();
    $('#b-toggle-sidebar').click(function () {
        $('.sidebar').toggle();
    });
}

function populateUI() {
    // days
    days = exampleUserString.days;
    daysList = $('.dayslist')
    for (i = 0; i > days.length; i++) {
        day = days[i];
        day_li = document.createElement('li')
        // day_li_div = document.
        day_li.innerHtml = `<div class="day-header">
        <h4>Monday, 13th of March 2015</h4>
        <div class="attributes">
            <ul class="attributes-list">
                <li class="attribute">X</li>
                <li class="attribute">A</li>
            </ul>
        </div>
        <p class="day-description">${day.description}</p>
    </div>`
    }
}


attachEvenListeners();
// populateUI();